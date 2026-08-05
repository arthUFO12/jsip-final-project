(* Tests for {!Client_logic.Sim_series} assembly on a small synthetic result. *)

open! Core
open Types
open Client_logic

let tick ~time_s ~cash ~realized ~unrealized ~yes_prices ~inventory =
  { Protocol.Tick_point.time_s
  ; cash
  ; realized
  ; unrealized
  ; yes_prices = List.map yes_prices ~f:(fun (s, p) -> Slug.of_string s, p)
  ; inventory = List.map inventory ~f:(fun (s, n) -> Slug.of_string s, n)
  }
;;

let result : Protocol.Sim_result.t =
  { ticks =
      [ tick
          ~time_s:0.
          ~cash:100.
          ~realized:0.
          ~unrealized:0.
          ~yes_prices:[ "aaa", 0.5 ]
          ~inventory:[ "aaa", 0 ]
      ; tick
          ~time_s:60.
          ~cash:99.5
          ~realized:0.
          ~unrealized:0.1
          ~yes_prices:[ "aaa", 0.6; "bbb", 0.2 ]
          ~inventory:[ "aaa", 1 ]
      ; tick
          ~time_s:120.
          ~cash:99.5
          ~realized:0.2
          ~unrealized:(-0.1)
          ~yes_prices:[ "aaa", 0.4; "bbb", 0.3 ]
          ~inventory:[ "aaa", 1; "bbb", -2 ]
      ]
  ; fills = []
  ; sim_start_s = 60.
  ; baseline_ticks =
      [ { Protocol.Baseline_point.time_s = 0.
        ; cash = 100.
        ; realized = 0.
        ; unrealized = 0.
        ; portfolio_value = 0.
        }
      ; { Protocol.Baseline_point.time_s = 120.
        ; cash = 99.
        ; realized = 0.5
        ; unrealized = 0.25
        ; portfolio_value = 1.
        }
      ]
  ; pnl_percentile = Some 80.
  ; truncated = false
  }
;;

let%expect_test "series assemble with portfolio marked per tick" =
  let series = Sim_series.create result ~max_points:800 in
  print_s [%sexp (series : Sim_series.t)];
  (* Portfolio at t=120: long 1 aaa at 0.4 (+0.4), short 2 bbb at 1 - 0.3
     (+1.4) = 1.8; total value = cash + portfolio. Slug "bbb" has no
     inventory entry at t=60, and no price at t=0 — both degrade, nothing
     raises. *)
  [%expect
    {|
    ((pnl
      (((name realized) (points ((0 0) (60 0) (120 0.2))))
       ((name unrealized) (points ((0 0) (60 0.1) (120 -0.1))))
       ((name total) (points ((0 0) (60 0.1) (120 0.1))))))
     (value
      (((name cash) (points ((0 100) (60 99.5) (120 99.5))))
       ((name "portfolio value")
        (points ((0 0) (60 0.6) (120 1.7999999999999998))))
       ((name "total value") (points ((0 100) (60 100.1) (120 101.3))))))
     (prices
      (((name aaa) (points ((0 0.5) (60 0.6) (120 0.4))))
       ((name bbb) (points ((60 0.2) (120 0.3))))))
     (inventory
      (((name aaa) (points ((0 0) (60 1) (120 1))))
       ((name bbb) (points ((120 -2))))))
     (baseline_pnl
      (((name "avg realized") (points ((0 0) (120 0.5))))
       ((name "avg unrealized") (points ((0 0) (120 0.25))))
       ((name "avg total") (points ((0 0) (120 0.75))))))
     (baseline_value
      (((name "avg cash") (points ((0 100) (120 99))))
       ((name "avg portfolio") (points ((0 0) (120 1))))
       ((name "avg total value") (points ((0 100) (120 100)))))))
    |}]
;;

let%expect_test "no baseline means empty baseline series" =
  let series =
    Sim_series.create
      { result with baseline_ticks = []; pnl_percentile = None }
      ~max_points:800
  in
  print_s
    [%sexp
      ((List.length series.baseline_pnl, List.length series.baseline_value)
       : int * int)];
  [%expect {| (0 0) |}]
;;
