(* Tests for {!Arbitrage.Replay} — the pure arb-backtest math. *)

open! Core
open Types
open Arbitrage

let price = Price.of_float_dollars

let%expect_test "edge is the cheaper split net of real fees" =
  (* Kalshi YES at 40c, Polymarket YES at 50c: buy YES on Kalshi + NO on
     Polymarket costs 0.40 + kalshi_fee(0.40) + 0.50 + 0 (polymarket fee
     is modeled zero). The other split costs 1.10 and loses. *)
  let edge =
    Replay.edge_after_fees
      ~venue_a:Kalshi
      ~yes_a:(price 0.40)
      ~venue_b:Polymarket
      ~yes_b:(price 0.50)
  in
  print_s [%sexp (edge : float)];
  [%expect {| 0.07999999999999996 |}];
  (* Same prices on both venues: no spread to capture, fees push the edge
     negative. *)
  let edge =
    Replay.edge_after_fees
      ~venue_a:Kalshi
      ~yes_a:(price 0.50)
      ~venue_b:Polymarket
      ~yes_b:(price 0.50)
  in
  print_s [%sexp (edge : float)];
  [%expect {| -0.020000000000000018 |}]
;;

let%expect_test "a persistent edge is taken once, then re-arms below threshold" =
  (* Edge holds above 5c for three ticks (one entry), collapses (re-arm),
     then spikes again (second entry) and is still open at the end. *)
  let points =
    [ 0., 0.02; 60., 0.06; 120., 0.07; 180., 0.06; 240., 0.01; 300., 0.09 ]
  in
  let episodes = Replay.episodes ~points ~min_edge:0.05 ~stake:10 in
  print_s [%sexp (episodes : Replay.Episode.t list)];
  [%expect
    {|
    (((entered_s 60) (exited_s (240)) (entry_edge 0.06) (locked_dollars 0.6))
     ((entered_s 300) (exited_s ()) (entry_edge 0.09)
      (locked_dollars 0.89999999999999991)))
    |}];
  print_s [%sexp (Replay.cumulative episodes : (float * float) list)];
  [%expect {| ((60 0.6) (300 1.5)) |}]
;;

let%expect_test "no edge, no episodes" =
  let points = [ 0., -0.01; 60., 0.005 ] in
  print_s
    [%sexp
      (Replay.episodes ~points ~min_edge:0.01 ~stake:10
       : Replay.Episode.t list)];
  [%expect {| () |}];
  print_s [%sexp (Replay.cumulative [] : (float * float) list)];
  [%expect {| () |}]
;;

let%expect_test "sightings collapse into episodes per pair, first dollars count" =
  (* Pair A seen at t=0,60,120 (one episode), gone, back at t=1200 (second
     episode). Pair B seen once at t=90. Gap threshold 300s. *)
  let sightings =
    [ 0., "A", 5.; 60., "A", 6.; 120., "A", 4.; 1200., "A", 2.; 90., "B", 3. ]
  in
  print_s
    [%sexp
      (Replay.observation_episodes ~sightings ~gap_s:300.
       : (float * float) list)];
  [%expect {| ((0 5) (90 8) (1200 10)) |}]
;;
