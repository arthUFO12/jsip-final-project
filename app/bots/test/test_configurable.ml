(* Tests for {!Bots.Configurable} with rules built directly in OCaml, driven
   by {!Harness_replica}. *)

open! Core

let slug = Harness_replica.slug
let drive = Harness_replica.drive

let buy ctx ~size : Parser.Rule.Action_spec.t =
  { side = Buy; size = Parser.Expr.Num.const ctx size; slug; contract = Yes }
;;

let%expect_test "EVERY 2h bare action fires on ticks 0, 2, 4" =
  let ctx = Parser.Expr.Context.create () in
  let rule =
    Parser.Rule.create
      ~every:(Some (Time_ns.Span.of_hr 2.))
      ~when_i:None
      (Always (buy ctx ~size:5.))
    |> Or_error.ok_exn
  in
  drive
    ~rules:[ rule ]
    ~yes_prices:[ 0.40; 0.40; 0.40; 0.40; 0.40 ]
    ~warmup:0
    ~initial_cash_dollars:100.
    ();
  [%expect
    {|
    2026-01-01 00:00:00.000000000Z accepted #1 buy 5 yes on save-act
    2026-01-01 02:00:00.000000000Z accepted #2 buy 5 yes on save-act
    2026-01-01 04:00:00.000000000Z accepted #3 buy 5 yes on save-act
    final: cash $94.00 | realized $0.00
    |}]
;;

let%expect_test "WHEN I BUY fires on the tick after the accepted fill" =
  let ctx = Parser.Expr.Context.create () in
  let seed =
    Parser.Rule.create
      ~every:(Some (Time_ns.Span.of_hr 4.))
      ~when_i:None
      (Always (buy ctx ~size:2.))
    |> Or_error.ok_exn
  in
  let unwind =
    Parser.Rule.create
      ~every:None
      ~when_i:(Some { side = Buy; slug })
      (Always { (buy ctx ~size:2.) with side = Sell })
    |> Or_error.ok_exn
  in
  drive
    ~rules:[ seed; unwind ]
    ~yes_prices:[ 0.40; 0.40; 0.40; 0.40; 0.40; 0.40 ]
    ~warmup:0
    ~initial_cash_dollars:100.
    ();
  [%expect
    {|
    2026-01-01 00:00:00.000000000Z accepted #1 buy 2 yes on save-act
    2026-01-01 01:00:00.000000000Z accepted #2 sell 2 yes on save-act
    2026-01-01 04:00:00.000000000Z accepted #3 buy 2 yes on save-act
    2026-01-01 05:00:00.000000000Z accepted #4 sell 2 yes on save-act
    final: cash $100.00 | realized $0.00
    |}]
;;

let%expect_test "builtin $cash gates a conditional rule" =
  let ctx = Parser.Expr.Context.create () in
  let var_env = Parser.Var_env.create ctx in
  let cash = Parser.Var_env.find_num var_env "cash" |> Or_error.ok_exn in
  let condition =
    Parser.Expr.Bool.cmp ctx Gt cash (Parser.Expr.Num.const ctx 98.)
  in
  let rule =
    Parser.Rule.create
      ~every:None
      ~when_i:None
      (Conditional
         { condition
         ; then_ = buy ctx ~size:5.
         ; else_ = Some { (buy ctx ~size:5.) with side = Sell }
         })
    |> Or_error.ok_exn
  in
  drive
    ~rules:[ rule ]
    ~yes_prices:[ 0.40; 0.40; 0.40; 0.40 ]
    ~warmup:0
    ~initial_cash_dollars:100.
    ();
  [%expect
    {|
    2026-01-01 00:00:00.000000000Z accepted #1 buy 5 yes on save-act
    2026-01-01 01:00:00.000000000Z accepted #2 sell 5 yes on save-act
    2026-01-01 02:00:00.000000000Z accepted #3 buy 5 yes on save-act
    2026-01-01 03:00:00.000000000Z accepted #4 sell 5 yes on save-act
    final: cash $100.00 | realized $0.00
    |}]
;;

let%expect_test "a price reference reacts to the moving market" =
  let ctx = Parser.Expr.Context.create () in
  let price = Parser.Expr.Num.price ctx ~slug in
  let condition =
    Parser.Expr.Bool.cmp ctx Gt price (Parser.Expr.Num.const ctx 0.45)
  in
  let rule =
    Parser.Rule.create
      ~every:None
      ~when_i:None
      (Conditional { condition; then_ = buy ctx ~size:1.; else_ = None })
    |> Or_error.ok_exn
  in
  drive
    ~rules:[ rule ]
    ~yes_prices:[ 0.30; 0.40; 0.50; 0.60 ]
    ~warmup:0
    ~initial_cash_dollars:100.
    ();
  [%expect
    {|
    2026-01-01 02:00:00.000000000Z accepted #1 buy 1 yes on save-act
    2026-01-01 03:00:00.000000000Z accepted #2 buy 1 yes on save-act
    final: cash $98.90 | realized $0.00
    |}]
;;

let%expect_test "a negative computed size is rejected and the sim continues" =
  let ctx = Parser.Expr.Context.create () in
  let rule =
    Parser.Rule.create
      ~every:(Some (Time_ns.Span.of_hr 2.))
      ~when_i:None
      (Always (buy ctx ~size:(-3.)))
    |> Or_error.ok_exn
  in
  drive
    ~rules:[ rule ]
    ~yes_prices:[ 0.40; 0.40; 0.40 ]
    ~warmup:0
    ~initial_cash_dollars:100.
    ();
  [%expect
    {|
    2026-01-01 00:00:00.000000000Z REJECTED #1 buy -3 yes on save-act: size must be non-negative; direction is carried by side
    2026-01-01 02:00:00.000000000Z REJECTED #2 buy -3 yes on save-act: size must be non-negative; direction is carried by side
    final: cash $100.00 | realized $0.00
    |}]
;;
