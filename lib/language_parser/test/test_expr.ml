(* Tests for {!Parser.Expr}: arithmetic, hash-consing, per-tick memoization,
   and short-circuit evaluation. Environments are hand-built
   {!Parser.Eval_env.t} records with counting closures, so everything runs
   without market data. *)

open! Core
open Types
open Parser

let slug = Slug.of_string "save-act"

(* An env whose [price_ago] counts its calls and reads from a mutable price,
   so tests can observe how often (and whether) the DAG touches prices. *)
let counting_env ?(price = 0.40) () =
  let calls = ref 0 in
  let price = ref price in
  let env : Eval_env.t =
    { cash = 100.
    ; realized = 0.
    ; unrealized = 0.
    ; price_ago =
        (fun ~slug:_ ~contract:_ ~ago:_ ->
          incr calls;
          Some !price)
    ; inventory = (fun ~slug:_ -> 4.)
    }
  in
  env, calls, price
;;

let%expect_test "arithmetic and comparisons" =
  let ctx = Expr.Context.create () in
  let num = Expr.Num.const ctx in
  (* (2 + 3) * 4 - 6 / 2 = 17 *)
  let expr =
    Expr.Num.sub
      ctx
      (Expr.Num.mul ctx (Expr.Num.add ctx (num 2.) (num 3.)) (num 4.))
      (Expr.Num.div ctx (num 6.) (num 2.))
  in
  let env, _, _ = counting_env () in
  let eval = Expr.Eval.create env in
  print_s [%sexp (Expr.Eval.num eval expr : float)];
  [%expect {| 17 |}];
  let ge = Expr.Bool.cmp ctx Ge expr (num 17.) in
  let ne = Expr.Bool.cmp ctx Ne expr (num 17.) in
  print_s
    [%message
      ""
        ~ge:(Expr.Eval.bool eval ge : bool)
        ~ne:(Expr.Eval.bool eval ne : bool)];
  [%expect {| ((ge true) (ne false)) |}]
;;

let%expect_test "builtins read the environment" =
  let ctx = Expr.Context.create () in
  let spendable =
    Expr.Bool.cmp ctx Gt (Expr.Num.cash ctx) (Expr.Num.const ctx 99.)
  in
  let env, _, _ = counting_env () in
  let eval = Expr.Eval.create env in
  print_s [%sexp (Expr.Eval.bool eval spendable : bool)];
  [%expect {| true |}]
;;

let%expect_test "hash-consing: equal constructions are the same node" =
  let ctx = Expr.Context.create () in
  let price () = Expr.Num.price ctx ~slug in
  let double () = Expr.Num.mul ctx (price ()) (Expr.Num.const ctx 2.) in
  print_s
    [%message
      ""
        ~same_leaf:(Expr.Num.id (price ()) = Expr.Num.id (price ()) : bool)
        ~same_tree:(Expr.Num.id (double ()) = Expr.Num.id (double ()) : bool)];
  [%expect {| ((same_leaf true) (same_tree true)) |}]
;;

let%expect_test "shared nodes evaluate once per tick" =
  let ctx = Expr.Context.create () in
  let price = Expr.Num.price ctx ~slug in
  (* Two roots both read the same price node. *)
  let cheap = Expr.Bool.cmp ctx Gt price (Expr.Num.const ctx 0.30) in
  let rich = Expr.Bool.cmp ctx Lt price (Expr.Num.const ctx 0.50) in
  let env, calls, _ = counting_env () in
  let eval = Expr.Eval.create env in
  let results = Expr.Eval.bool eval cheap, Expr.Eval.bool eval rich in
  print_s
    [%message
      "" ~results:(results : bool * bool) ~price_lookups:(!calls : int)];
  [%expect {| ((results (true true)) (price_lookups 1)) |}]
;;

let%expect_test "and/or short-circuit without touching the right side" =
  let ctx = Expr.Context.create () in
  let price_check =
    Expr.Bool.cmp ctx Gt (Expr.Num.price ctx ~slug) (Expr.Num.const ctx 0.30)
  in
  let never = Expr.Bool.and_ ctx (Expr.Bool.const ctx false) price_check in
  let always = Expr.Bool.or_ ctx (Expr.Bool.const ctx true) price_check in
  let env, calls, _ = counting_env () in
  let eval = Expr.Eval.create env in
  let results = Expr.Eval.bool eval never, Expr.Eval.bool eval always in
  print_s
    [%message
      "" ~results:(results : bool * bool) ~price_lookups:(!calls : int)];
  [%expect {| ((results (false true)) (price_lookups 0)) |}]
;;

let%expect_test "the memo table is per-tick, not per-program" =
  let ctx = Expr.Context.create () in
  let cheap =
    Expr.Bool.cmp ctx Lt (Expr.Num.price ctx ~slug) (Expr.Num.const ctx 0.50)
  in
  let env, _, price = counting_env () in
  let tick_one = Expr.Eval.create env in
  let before = Expr.Eval.bool tick_one cheap in
  price := 0.60;
  (* The old eval still answers from its memo; a fresh eval sees the new
     price. *)
  let stale = Expr.Eval.bool tick_one cheap in
  let tick_two = Expr.Eval.create env in
  let after = Expr.Eval.bool tick_two cheap in
  print_s [%message "" (before : bool) (stale : bool) (after : bool)];
  [%expect {| ((before true) (stale true) (after false)) |}]
;;

let%expect_test "inventory references intern and read the environment" =
  let ctx = Expr.Context.create () in
  let first = Expr.Num.inventory ctx ~slug in
  let second = Expr.Num.inventory ctx ~slug in
  let env, _, _ = counting_env () in
  let eval = Expr.Eval.create env in
  print_s
    [%message
      ""
        ~same_node:(Expr.Num.id first = Expr.Num.id second : bool)
        ~held:(Expr.Eval.num eval first : float)
        ~slugs:(Expr.Num.referenced_slugs first : Slug.t list)];
  [%expect {| ((same_node true) (held 4) (slugs (save-act))) |}]
;;
