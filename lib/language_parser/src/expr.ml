open! Core
open Types

module Cmp = struct
  type t =
    | Gt
    | Lt
    | Ge
    | Le
    | Eq
    | Ne
  [@@deriving sexp_of, compare, equal, hash]

  let apply t left right =
    match t with
    | Gt -> Float.O.(left > right)
    | Lt -> Float.O.(left < right)
    | Ge -> Float.O.(left >= right)
    | Le -> Float.O.(left <= right)
    | Eq -> Float.O.(left = right)
    | Ne -> Float.O.(left <> right)
  ;;
end

module Num_op = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div
  [@@deriving sexp_of, compare, equal, hash]

  let apply t left right =
    match t with
    | Add -> left +. right
    | Sub -> left -. right
    | Mul -> left *. right
    | Div -> left /. right
  ;;
end

module Logic_op = struct
  type t =
    | And
    | Or
    | Xor
  [@@deriving sexp_of, compare, equal, hash]
end

type num =
  { num_id : int
  ; num_node : num_node
  }

and num_node =
  | Const of float
  | Cash
  | Realized
  | Unrealized
  | Price_of of Slug.t
  | Inventory_of of Slug.t
  | Avgcost_of of Slug.t
  | Num_binop of Num_op.t * num * num
  | Neg of num

and bool_ =
  { bool_id : int
  ; bool_node : bool_node
  }

and bool_node =
  | Bool_const of bool
  | Logic of Logic_op.t * bool_ * bool_
  | Not of bool_
  | Cmp_node of Cmp.t * num * num
  | Signal_leaf of Market_signal.t
[@@deriving sexp_of]

(* Hash-consing keys represent children by id, so hashing a key is O(1)
   regardless of how deep the expression is. *)
module Num_key = struct
  module T = struct
    type t =
      | Const of float
      | Cash
      | Realized
      | Unrealized
      | Price_of of Slug.t
      | Inventory_of of Slug.t
      | Avgcost_of of Slug.t
      | Binop of Num_op.t * int * int
      | Neg of int
    [@@deriving sexp_of, compare, hash]
  end

  include T
  include Hashable.Make_plain (T)
end

module Bool_key = struct
  module T = struct
    type t =
      | Const of bool
      | Logic of Logic_op.t * int * int
      | Not of int
      | Cmp of Cmp.t * int * int
      | Signal of Market_signal.t
    [@@deriving sexp_of, compare, hash]
  end

  include T
  include Hashable.Make_plain (T)
end

module Context = struct
  type t =
    { mutable next_id : int
    ; nums : num Num_key.Table.t
    ; bools : bool_ Bool_key.Table.t
    }

  let create () =
    { next_id = 0
    ; nums = Num_key.Table.create ()
    ; bools = Bool_key.Table.create ()
    }
  ;;

  let fresh_id t =
    let id = t.next_id in
    t.next_id <- id + 1;
    id
  ;;

  let intern_num t key ~node =
    Hashtbl.find_or_add t.nums key ~default:(fun () ->
      { num_id = fresh_id t; num_node = node () })
  ;;

  let intern_bool t key ~node =
    Hashtbl.find_or_add t.bools key ~default:(fun () ->
      { bool_id = fresh_id t; bool_node = node () })
  ;;
end

module Num = struct
  type t = num [@@deriving sexp_of]

  let id t = t.num_id

  let const ctx value =
    Context.intern_num ctx (Const value) ~node:(fun () -> Const value)
  ;;

  let cash ctx = Context.intern_num ctx Cash ~node:(fun () -> Cash)

  let realized ctx =
    Context.intern_num ctx Realized ~node:(fun () -> Realized)
  ;;

  let unrealized ctx =
    Context.intern_num ctx Unrealized ~node:(fun () -> Unrealized)
  ;;

  let price ctx ~slug =
    Context.intern_num ctx (Price_of slug) ~node:(fun () -> Price_of slug)
  ;;

  let inventory ctx ~slug =
    Context.intern_num ctx (Inventory_of slug) ~node:(fun () ->
      Inventory_of slug)
  ;;

  let avgcost ctx ~slug =
    Context.intern_num ctx (Avgcost_of slug) ~node:(fun () ->
      Avgcost_of slug)
  ;;

  let binop ctx op left right =
    Context.intern_num
      ctx
      (Binop (op, left.num_id, right.num_id))
      ~node:(fun () -> Num_binop (op, left, right))
  ;;

  let add ctx = binop ctx Add
  let sub ctx = binop ctx Sub
  let mul ctx = binop ctx Mul
  let div ctx = binop ctx Div

  let neg ctx operand =
    Context.intern_num ctx (Neg operand.num_id) ~node:(fun () -> Neg operand)
  ;;

  let iter_slugs t ~visited ~f =
    let rec walk (t : num) =
      match Hash_set.mem visited t.num_id with
      | true -> ()
      | false ->
        Hash_set.add visited t.num_id;
        (match t.num_node with
         | Const _ | Cash | Realized | Unrealized -> ()
         | Price_of slug | Inventory_of slug | Avgcost_of slug -> f slug
         | Num_binop (_, left, right) ->
           walk left;
           walk right
         | Neg operand -> walk operand)
    in
    walk t
  ;;

  let referenced_slugs t =
    let visited = Int.Hash_set.create () in
    let slugs = Slug.Hash_set.create () in
    iter_slugs t ~visited ~f:(Hash_set.add slugs);
    Hash_set.to_list slugs
  ;;
end

module Bool = struct
  type t = bool_ [@@deriving sexp_of]

  let id t = t.bool_id

  let const ctx value =
    Context.intern_bool ctx (Const value) ~node:(fun () -> Bool_const value)
  ;;

  let logic ctx op left right =
    Context.intern_bool
      ctx
      (Logic (op, left.bool_id, right.bool_id))
      ~node:(fun () -> Logic (op, left, right))
  ;;

  let and_ ctx = logic ctx And
  let or_ ctx = logic ctx Or
  let xor ctx = logic ctx Xor

  let not_ ctx operand =
    Context.intern_bool ctx (Not operand.bool_id) ~node:(fun () ->
      Not operand)
  ;;

  let cmp ctx op left right =
    Context.intern_bool
      ctx
      (Cmp (op, Num.id left, Num.id right))
      ~node:(fun () -> Cmp_node (op, left, right))
  ;;

  let signal ctx signal =
    Context.intern_bool ctx (Signal signal) ~node:(fun () ->
      Signal_leaf signal)
  ;;

  let iter_leaves t ~f_slug ~f_signal =
    let visited = Int.Hash_set.create () in
    let rec walk (t : bool_) =
      match Hash_set.mem visited t.bool_id with
      | true -> ()
      | false ->
        Hash_set.add visited t.bool_id;
        (match t.bool_node with
         | Bool_const _ -> ()
         | Logic (_, left, right) ->
           walk left;
           walk right
         | Not operand -> walk operand
         | Cmp_node (_, left, right) ->
           Num.iter_slugs left ~visited ~f:f_slug;
           Num.iter_slugs right ~visited ~f:f_slug
         | Signal_leaf signal -> f_signal signal)
    in
    walk t
  ;;

  let referenced_slugs t =
    let slugs = Slug.Hash_set.create () in
    iter_leaves t ~f_slug:(Hash_set.add slugs) ~f_signal:(fun signal ->
      Hash_set.add slugs (Market_signal.slug signal));
    Hash_set.to_list slugs
  ;;

  let signals t =
    let signals = Market_signal.Hash_set.create () in
    iter_leaves
      t
      ~f_slug:(fun (_ : Slug.t) -> ())
      ~f_signal:(Hash_set.add signals);
    Hash_set.to_list signals
  ;;
end

module Eval = struct
  type t =
    { env : Eval_env.t
    ; nums : float Int.Table.t
    ; bools : bool Int.Table.t
    }

  let create env =
    { env; nums = Int.Table.create (); bools = Int.Table.create () }
  ;;

  let current_yes_price t slug =
    match t.env.price_ago ~slug ~contract:Yes ~ago:Time_ns.Span.zero with
    | Some price -> price
    | None ->
      (* Program validation guarantees every referenced slug is in the sim,
         and the harness feeds a point for every slug before [on_tick]. *)
      raise_s
        [%message "no current price for slug in simulation" (slug : Slug.t)]
  ;;

  let rec num t (node : num) =
    match Hashtbl.find t.nums node.num_id with
    | Some value -> value
    | None ->
      let value =
        match node.num_node with
        | Const value -> value
        | Cash -> t.env.cash
        | Realized -> t.env.realized
        | Unrealized -> t.env.unrealized
        | Price_of slug -> current_yes_price t slug
        | Inventory_of slug -> t.env.inventory ~slug
        | Avgcost_of slug -> t.env.avg_cost ~slug
        | Num_binop (op, left, right) ->
          Num_op.apply op (num t left) (num t right)
        | Neg operand -> -.num t operand
      in
      Hashtbl.set t.nums ~key:node.num_id ~data:value;
      value
  ;;

  let rec bool t (node : bool_) =
    match Hashtbl.find t.bools node.bool_id with
    | Some value -> value
    | None ->
      let value =
        match node.bool_node with
        | Bool_const value -> value
        | Logic (And, left, right) -> bool t left && bool t right
        | Logic (Or, left, right) -> bool t left || bool t right
        | Logic (Xor, left, right) ->
          Core.Bool.( <> ) (bool t left) (bool t right)
        | Not operand -> not (bool t operand)
        | Cmp_node (op, left, right) ->
          Cmp.apply op (num t left) (num t right)
        | Signal_leaf signal -> Market_signal.evaluate signal ~env:t.env
      in
      Hashtbl.set t.bools ~key:node.bool_id ~data:value;
      value
  ;;
end

module Atom = struct
  type t =
    { negated : bool
    ; description : string
    }

  let to_string { negated; description } =
    match negated with
    | false -> description
    | true -> [%string "not (%{description})"]
  ;;
end

(* %g keeps 0.05 as "0.05" and 50. as "50" — the spellings a program uses. *)
let float_string value = sprintf "%g" value

(* Program syntax for a numeric operand, with the live value of each state
   reference in parentheses. Only called on subtrees the rule already
   evaluated this tick, so [Eval.num] is a memo hit and cannot raise. *)
let rec num_description eval (node : num) =
  let live () = float_string (Eval.num eval node) in
  match node.num_node with
  | Const value -> float_string value
  | Cash -> [%string "$cash (%{live ()})"]
  | Realized -> [%string "$realized (%{live ()})"]
  | Unrealized -> [%string "$unrealized (%{live ()})"]
  | Price_of slug ->
    let name = Ticker_name.normalize (Slug.to_string slug) in
    [%string "price of %{name} (%{live ()})"]
  | Inventory_of slug ->
    let name = Ticker_name.normalize (Slug.to_string slug) in
    [%string "inventory of %{name} (%{live ()})"]
  | Avgcost_of slug ->
    let name = Ticker_name.normalize (Slug.to_string slug) in
    [%string "avgcost of %{name} (%{live ()})"]
  | Num_binop (op, left, right) ->
    let op =
      match op with
      | Num_op.Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
    in
    [%string
      "(%{num_description eval left} %{op} %{num_description eval right})"]
  | Neg operand -> [%string "-%{num_description eval operand}"]
;;

let cmp_description eval op left right =
  let op =
    match (op : Cmp.t) with
    | Gt -> ">"
    | Lt -> "<"
    | Ge -> ">="
    | Le -> "<="
    | Eq -> "=="
    | Ne -> "!="
  in
  [%string
    "%{num_description eval left} %{op} %{num_description eval right}"]
;;

let justify bool_expr eval ~want : Atom.t list =
  let atoms = ref [] in
  let visited = Int.Hash_set.create () in
  (* [want] always equals the node's actual value (the walk only descends
     branches consistent with how the rule evaluated), so a leaf reached with
     [want = false] is reported negated. *)
  let emit (node : bool_) ~want =
    match Hash_set.mem visited node.bool_id with
    | true -> ()
    | false ->
      Hash_set.add visited node.bool_id;
      (match node.bool_node with
       (* A literal [true]/[false] is not an event worth reporting. *)
       | Bool_const _ | Logic _ | Not _ -> ()
       | Signal_leaf signal ->
         atoms
         := { Atom.negated = not want
            ; description = Market_signal.to_string signal
            }
            :: !atoms
       | Cmp_node (op, left, right) ->
         atoms
         := { Atom.negated = not want
            ; description = cmp_description eval op left right
            }
            :: !atoms)
  in
  let rec walk (node : bool_) ~want =
    match node.bool_node with
    | Bool_const _ | Cmp_node _ | Signal_leaf _ -> emit node ~want
    | Not operand -> walk operand ~want:(not want)
    | Logic (And, left, right) ->
      (match want with
       | true ->
         (* Both conjuncts were needed. *)
         walk left ~want:true;
         walk right ~want:true
       | false ->
         (* The first false conjunct explains it, mirroring the evaluator's
            short-circuit order. *)
         (match Eval.bool eval left with
          | false -> walk left ~want:false
          | true -> walk right ~want:false))
    | Logic (Or, left, right) ->
      (match want with
       | true ->
         (match Eval.bool eval left with
          | true -> walk left ~want:true
          | false -> walk right ~want:true)
       | false ->
         walk left ~want:false;
         walk right ~want:false)
    | Logic (Xor, left, right) ->
      (* No short circuit: both sides always matter, each at its actual
         value. *)
      walk left ~want:(Eval.bool eval left);
      walk right ~want:(Eval.bool eval right)
  in
  walk bool_expr ~want;
  List.rev !atoms
;;
