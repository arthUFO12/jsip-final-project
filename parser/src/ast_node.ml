open! Core 

module T = struct
type t = 
| And of t * t
| Or of t * t
| Xor of t * t
| Not of t
| Leaf of Market_signal.t
[@@deriving sexp, bin_io, compare, equal, hash]
end 

include T
include Hashable.Make (T)




module Context = struct
  type t = {
    ids : T.t Int.Table.t
  ; nodes_to_ids : int Table.t
  ; mutable counter : int
  }

  let _create () = {
    ids = Int.Table.create ()
  ; nodes_to_ids = Table.create ()
  ; counter = 0
  }
end

let find_or_add_node (context : Context.t) node =
  match Hashtbl.find context.nodes_to_ids node with 
    | None ->
    Hashtbl.set context.ids ~key:context.counter ~data:node;
    Hashtbl.set context.nodes_to_ids ~key:node ~data:context.counter;
    context.counter <- context.counter + 1;
    node
    | Some id ->
    Hashtbl.find_exn context.ids id
let _create_and_node (context : Context.t) ~left ~right =
  let node = And (left, right) in
  find_or_add_node context node
;;

let _create_or_node context ~left ~right =
  let node = Or (left, right) in
  find_or_add_node context node
;;

let _create_xor_node context ~left ~right =
  let node = Xor (left, right) in
  find_or_add_node context node
;;

let _create_not_node context pred = 
  let node = Not pred in
  find_or_add_node context node
;;

let _create_leaf_node context signal =
  let node = Leaf signal in
  find_or_add_node context node
;;
let rec evaluate_node (t : t) : bool = 
  match t with
    | And (left, right) -> (evaluate_node left) && (evaluate_node right)
    | Or (left, right) -> (evaluate_node left) || (evaluate_node right)
    | Xor (left, right) -> Bool.(<>) (evaluate_node left) (evaluate_node right)
    | Not pred -> not (evaluate_node pred)
    | Leaf signal -> Market_signal.evaluate_signal signal 0