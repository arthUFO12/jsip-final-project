(* True end-to-end tests: pseudo-language text -> {!Parser.Parse} ->
   {!Bots.Configurable} -> {!Harness_replica} backtest over synthetic hourly
   prices for the [save-act] market. *)

open! Core
open Parser

let run ?(warmup = 0) ?(cash = 100.) ?allow_negative_cash program ~yes_prices
  =
  match Parse.program program ~slugs:[ Harness_replica.slug ] with
  | Error error -> print_s [%sexp (error : Error.t)]
  | Ok rules ->
    Harness_replica.drive
      ?allow_negative_cash
      ~rules
      ~yes_prices
      ~warmup
      ~initial_cash_dollars:cash
      ()
;;

let%expect_test "momentum program: EVERY accumulation, signal-based exits" =
  run
    {|lot = 2 + 3
      EVERY 2h BUY $lot save-act YES
      IF save-act UP BY 0.15 SINCE 1h AGO THEN SELL 2 save-act YES|}
    ~yes_prices:[ 0.40; 0.60; 0.40; 0.60 ];
  (* Tick 0: EVERY fires; the signal has no 1h-ago history yet. Ticks 1 and
     3: the price jumped 0.20 >= 0.15, so the exit rule sells. *)
  [%expect
    {|
    2026-01-01 00:00:00.000000000Z accepted #1 buy 5 yes on save-act
    2026-01-01 01:00:00.000000000Z accepted #2 sell 2 yes on save-act
    2026-01-01 02:00:00.000000000Z accepted #3 buy 5 yes on save-act
    2026-01-01 03:00:00.000000000Z accepted #4 sell 2 yes on save-act
    final: cash $98.40 | realized $0.80
    |}]
;;

let%expect_test "warmup history feeds lookbacks; WHEN I chains next tick" =
  run
    ~warmup:2
    {|WHEN I BUY save-act SELL 1 save-act YES
      IF save-act DOWN BY 20% SINCE 2h AGO THEN BUY 2 save-act YES|}
    ~yes_prices:[ 0.50; 0.50; 0.40; 0.40; 0.30 ];
  (* The first simulated tick (02:00) already sees the two warmup points, so
     its 2h lookback works: 0.50 -> 0.40 is exactly a 20% drop. Each accepted
     buy triggers the WHEN I unwind on the following tick. *)
  [%expect
    {|
    2026-01-01 02:00:00.000000000Z accepted #1 buy 2 yes on save-act
    2026-01-01 03:00:00.000000000Z accepted #2 sell 1 yes on save-act
    2026-01-01 03:00:00.000000000Z accepted #3 buy 2 yes on save-act
    2026-01-01 04:00:00.000000000Z accepted #4 sell 1 yes on save-act
    2026-01-01 04:00:00.000000000Z accepted #5 buy 2 yes on save-act
    final: cash $98.50 | realized -$0.10
    |}]
;;

let%expect_test "INVENTORY caps the position at 3 contracts" =
  run
    "IF INVENTORY OF save-act < 3 THEN BUY 1 save-act YES"
    ~yes_prices:[ 0.40; 0.40; 0.40; 0.40; 0.40 ];
  (* The position grows by one per tick and the rule goes quiet once the
     inventory cached from [on_response] reaches 3. *)
  [%expect
    {|
    2026-01-01 00:00:00.000000000Z accepted #1 buy 1 yes on save-act
    2026-01-01 01:00:00.000000000Z accepted #2 buy 1 yes on save-act
    2026-01-01 02:00:00.000000000Z accepted #3 buy 1 yes on save-act
    final: cash $98.80 | realized $0.00
    |}]
;;

let%expect_test "AVGCOST take profit only sells above the entry price" =
  run
    {|EVERY 2h BUY 2 save-act YES
      IF INVENTORY OF save-act > 0 && PRICE OF save-act > AVGCOST OF save-act THEN SELL 1 save-act YES|}
    ~yes_prices:[ 0.40; 0.45; 0.35; 0.60 ];
  (* Tick 0: entry at 0.40. Tick 1: 0.45 beats the 0.40 average, sell one.
     Tick 2: 0.35 is below it — the EVERY buy fires but no sale. Tick 3: the
     average is now (1 x 0.40 + 2 x 0.35) / 3 ~ 0.37, so 0.60 sells. *)
  [%expect
    {|
    2026-01-01 00:00:00.000000000Z accepted #1 buy 2 yes on save-act
    2026-01-01 01:00:00.000000000Z accepted #2 sell 1 yes on save-act
    2026-01-01 02:00:00.000000000Z accepted #3 buy 2 yes on save-act
    2026-01-01 03:00:00.000000000Z accepted #4 sell 1 yes on save-act
    final: cash $99.55 | realized $0.28333334
    |}]
;;

let%expect_test "the cash gate rejects enlarging trades that would overdraw" =
  (* $10 of cash cannot fund a $40 buy; with the gate on, every attempt is
     rejected and the book never moves. *)
  run
    ~cash:10.
    ~allow_negative_cash:false
    "EVERY 1h BUY 100 save-act YES"
    ~yes_prices:[ 0.40; 0.40; 0.40 ];
  [%expect
    {|
    2026-01-01 00:00:00.000000000Z REJECTED #1 buy 100 yes on save-act: insufficient funds for trade
    2026-01-01 01:00:00.000000000Z REJECTED #2 buy 100 yes on save-act: insufficient funds for trade
    2026-01-01 02:00:00.000000000Z REJECTED #3 buy 100 yes on save-act: insufficient funds for trade
    final: cash $10.00 | realized $0.00
    |}]
;;

let%expect_test "reducing trades pass the gate even with cash pinned low" =
  (* The opening buy takes cash down to $2; the hourly unwinds still go
     through because selling out of a long never enlarges the position. *)
  run
    ~cash:10.
    ~allow_negative_cash:false
    {|EVERY 3h BUY 20 save-act YES
      EVERY 1h IF INVENTORY OF save-act > 0 THEN SELL 10 save-act YES|}
    ~yes_prices:[ 0.40; 0.40; 0.40 ];
  [%expect
    {|
    2026-01-01 00:00:00.000000000Z accepted #1 buy 20 yes on save-act
    2026-01-01 01:00:00.000000000Z accepted #2 sell 10 yes on save-act
    2026-01-01 02:00:00.000000000Z accepted #3 sell 10 yes on save-act
    final: cash $10.00 | realized $0.00
    |}]
;;

let%expect_test "buy every 1 hour and sell if the price drops below $0.30" =
  let program =
    {|EVERY 1h BUY 100 save-act YES
IF save-act < 0.30 THEN SELL 600 save-act YES|}
  in
  run program ~yes_prices:[ 0.30; 0.35; 0.45; 0.60; 0.55; 0.20 ];
  [%expect
    {|
    2026-01-01 00:00:00.000000000Z accepted #1 buy 100 yes on save-act
    2026-01-01 01:00:00.000000000Z accepted #2 buy 100 yes on save-act
    2026-01-01 02:00:00.000000000Z accepted #3 buy 100 yes on save-act
    2026-01-01 03:00:00.000000000Z accepted #4 buy 100 yes on save-act
    2026-01-01 04:00:00.000000000Z accepted #5 buy 100 yes on save-act
    2026-01-01 05:00:00.000000000Z accepted #6 buy 100 yes on save-act
    2026-01-01 05:00:00.000000000Z accepted #7 sell 600 yes on save-act
    final: cash -$25.00 | realized -$125.00
    |}]
;;
