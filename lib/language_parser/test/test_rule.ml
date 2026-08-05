(* Tests for {!Parser.Rule} bodies/qualifier validation and {!Parser.Program}
   compilation checks. *)

open! Core
open Types
open Parser

let slug = Slug.of_string "save-act"
let other = Slug.of_string "other-market"
let hour = Time_ns.Span.of_hr 1.

let buy ctx ~size : Rule.Action_spec.t =
  { side = Buy; size = Expr.Num.const ctx size; slug; contract = Yes }
;;

let env_with_price price : Eval_env.t =
  { cash = 0.
  ; realized = 0.
  ; unrealized = 0.
  ; price_ago = (fun ~slug:_ ~contract:_ ~ago:_ -> Some price)
  ; inventory = (fun ~slug:_ -> 0.)
  ; avg_cost = (fun ~slug:_ -> 0.)
  }
;;

let%expect_test "bare actions require a qualifier" =
  let ctx = Expr.Context.create () in
  let body = Rule.Body.Always (buy ctx ~size:1.) in
  let bare = Rule.create ~every:None ~when_i:None body in
  print_s [%sexp (Or_error.map bare ~f:(fun _ -> "ok") : string Or_error.t)];
  [%expect
    {| (Error "a bare action statement requires an EVERY or WHEN I qualifier") |}];
  let qualified =
    Rule.create ~every:(Some hour) ~when_i:None body
    |> Or_error.map ~f:(fun _ -> "ok")
  in
  print_s [%sexp (qualified : string Or_error.t)];
  [%expect {| (Ok ok) |}];
  let negative =
    Rule.create ~every:(Some (Time_ns.Span.of_hr (-1.))) ~when_i:None body
    |> Or_error.map ~f:(fun _ -> "ok")
  in
  print_s [%sexp (negative : string Or_error.t)];
  [%expect {| (Error ("EVERY interval must be positive" (span -1h))) |}]
;;

let%expect_test "conditional bodies pick then/else by the condition" =
  let ctx = Expr.Context.create () in
  let condition =
    Expr.Bool.cmp ctx Gt (Expr.Num.price ctx ~slug) (Expr.Num.const ctx 0.50)
  in
  let rule =
    Rule.create
      ~every:None
      ~when_i:None
      (Conditional
         { condition
         ; then_ = buy ctx ~size:1.
         ; else_ = Some { (buy ctx ~size:2.) with side = Sell }
         })
    |> Or_error.ok_exn
  in
  let side_at price =
    let eval = Expr.Eval.create (env_with_price price) in
    match Rule.eval rule eval with
    | None -> "none"
    | Some { side = Buy; _ } -> "buy"
    | Some { side = Sell; _ } -> "sell"
  in
  print_s
    [%message
      "" ~cheap:(side_at 0.40 : string) ~rich:(side_at 0.60 : string)];
  [%expect {| ((cheap sell) (rich buy)) |}]
;;

let%expect_test "program validation catches bad spans and foreign slugs" =
  let ctx = Expr.Context.create () in
  let bare_buy every =
    Rule.create ~every:(Some every) ~when_i:None (Always (buy ctx ~size:1.))
    |> Or_error.ok_exn
  in
  let compile rules =
    Program.create ~rules ~tick:hour ~slugs:[ slug ]
    |> Or_error.map ~f:(fun program ->
      List.map (Program.rules program) ~f:(fun compiled ->
        compiled.every_ticks))
  in
  print_s
    [%sexp
      (compile [ bare_buy (Time_ns.Span.of_hr 2.) ]
       : int option list Or_error.t)];
  [%expect {| (Ok ((2))) |}];
  print_s
    [%sexp
      (compile [ bare_buy (Time_ns.Span.of_min 90.) ]
       : int option list Or_error.t)];
  [%expect
    {|
    (Error
     ("span is not a whole number of simulation ticks" (what "EVERY interval")
      (span 1h30m) (tick 1h)))
    |}];
  let on_other =
    Rule.create
      ~every:(Some hour)
      ~when_i:None
      (Always { (buy ctx ~size:1.) with slug = other })
    |> Or_error.ok_exn
  in
  print_s [%sexp (compile [ on_other ] : int option list Or_error.t)];
  [%expect
    {|
    (Error
     ("rule references a market not in this simulation" (slug other-market)))
    |}];
  let ragged_window =
    let signal : Market_signal.t =
      Moved
        { slug
        ; contract = Yes
        ; direction = Down
        ; window = Market_signal.Window.since (Time_ns.Span.of_min 90.)
        ; by = Percent 3.
        }
    in
    Rule.create
      ~every:None
      ~when_i:None
      (Conditional
         { condition = Expr.Bool.signal ctx signal
         ; then_ = buy ctx ~size:1.
         ; else_ = None
         })
    |> Or_error.ok_exn
  in
  print_s [%sexp (compile [ ragged_window ] : int option list Or_error.t)];
  [%expect
    {|
    (Error
     ("span is not a whole number of simulation ticks"
      (what "signal window start_ago") (span 1h30m) (tick 1h)))
    |}]
;;
