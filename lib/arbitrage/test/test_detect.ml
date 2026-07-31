open! Core
open! Types
open Arbitrage

(* Books and legs built from cents/contracts so the scenarios read like a
   trading screen. *)
let book ~yes_ask ~yes_size ~no_ask ~no_size =
  { Binary_book.yes_ask = Price.of_int_cents yes_ask
  ; yes_ask_size = Size.of_int yes_size
  ; no_ask = Price.of_int_cents no_ask
  ; no_ask_size = Size.of_int no_size
  }
;;

let leg venue id book =
  { Detect.Leg.venue; market_id = Market_id.of_string id; book }
;;

let print_opportunity opportunity =
  print_s [%sexp (opportunity : Detect.opportunity option)]
;;

let%expect_test "kalshi_fee matches the published table, rounding up to a \
                 cent"
  =
  List.iter [ 0; 1; 10; 25; 33; 50; 75; 99 ] ~f:(fun cents ->
    let price = Price.of_int_cents cents in
    let fee = Detect.For_testing.kalshi_fee price in
    print_endline
      [%string
        "%{Price.to_string_dollar price} -> %{Price.to_string_dollar fee}"]);
  [%expect
    {|
    $0.00 -> $0.00
    $0.01 -> $0.01
    $0.10 -> $0.01
    $0.25 -> $0.02
    $0.33 -> $0.02
    $0.50 -> $0.02
    $0.75 -> $0.02
    $0.99 -> $0.01
    |}]
;;

let%expect_test "taker_fee: only Kalshi charges" =
  let price = Price.of_int_cents 50 in
  List.iter
    Venue.[ Kalshi; Polymarket ]
    ~f:(fun venue ->
      let fee = Detect.For_testing.taker_fee venue price in
      print_endline [%string "%{venue#Venue}: %{Price.to_string_dollar fee}"]);
  [%expect {|
    Kalshi: $0.02
    Polymarket: $0.00
    |}]
;;

let%expect_test "cost_and_size: asks + fees on the fee-charging leg, size \
                 is the thinner book"
  =
  let kalshi =
    leg Kalshi "K1" (book ~yes_ask:45 ~yes_size:100 ~no_ask:60 ~no_size:50)
  in
  let poly =
    leg
      Polymarket
      "P1"
      (book ~yes_ask:52 ~yes_size:30 ~no_ask:48 ~no_size:80)
  in
  let show ~yes ~no =
    let cost, size = Detect.For_testing.cost_and_size ~mode:Exact ~yes ~no in
    print_endline
      [%string
        "yes %{yes.Detect.Leg.venue#Venue} / no \
         %{no.Detect.Leg.venue#Venue}: cost %{Price.to_string_dollar cost}, \
         size %{size#Size}"]
  in
  (* 45c + 2c fee + 48c = 95c; sizes min(100, 80) = 80. *)
  show ~yes:kalshi ~no:poly;
  (* 52c + 60c + 2c fee = 114c; sizes min(30, 50) = 30. *)
  show ~yes:poly ~no:kalshi;
  [%expect
    {|
    yes Kalshi / no Polymarket: cost $0.95, size 80
    yes Polymarket / no Kalshi: cost $1.14, size 30
    |}]
;;

let%expect_test "find returns the profitable split and is symmetric in its \
                 arguments"
  =
  let kalshi =
    leg Kalshi "K1" (book ~yes_ask:45 ~yes_size:100 ~no_ask:60 ~no_size:50)
  in
  let poly =
    leg
      Polymarket
      "P1"
      (book ~yes_ask:52 ~yes_size:30 ~no_ask:48 ~no_size:80)
  in
  print_opportunity (Detect.find ~mode:Exact kalshi poly);
  print_opportunity (Detect.find ~mode:Exact poly kalshi);
  [%expect
    {|
    (((yes ((venue Kalshi) (market_id K1) (price 45000000)))
      (no ((venue Polymarket) (market_id P1) (price 48000000))) (cost 95000000)
      (edge 5000000) (size 80)))
    (((yes ((venue Kalshi) (market_id K1) (price 45000000)))
      (no ((venue Polymarket) (market_id P1) (price 48000000))) (cost 95000000)
      (edge 5000000) (size 80)))
    |}]
;;

let%expect_test "find: no edge when both splits cost a dollar or more" =
  let kalshi =
    leg Kalshi "K1" (book ~yes_ask:55 ~yes_size:100 ~no_ask:55 ~no_size:100)
  in
  let poly =
    leg
      Polymarket
      "P1"
      (book ~yes_ask:50 ~yes_size:100 ~no_ask:50 ~no_size:100)
  in
  print_opportunity (Detect.find ~mode:Exact kalshi poly);
  [%expect {| () |}]
;;

let%expect_test "find: a priced edge with no size behind it is not tradable" =
  let kalshi =
    leg Kalshi "K1" (book ~yes_ask:45 ~yes_size:0 ~no_ask:60 ~no_size:0)
  in
  let poly =
    leg
      Polymarket
      "P1"
      (book ~yes_ask:52 ~yes_size:30 ~no_ask:48 ~no_size:80)
  in
  print_opportunity (Detect.find ~mode:Exact kalshi poly);
  [%expect {| () |}]
;;

let%expect_test "find keeps the larger edge when both splits clear" =
  let kalshi =
    leg Kalshi "K1" (book ~yes_ask:40 ~yes_size:10 ~no_ask:45 ~no_size:10)
  in
  let poly =
    leg
      Polymarket
      "P1"
      (book ~yes_ask:45 ~yes_size:10 ~no_ask:40 ~no_size:10)
  in
  (* yes-Kalshi: 40c + 2c fee + 40c = 82c (edge 18c); yes-Poly: 45c + 45c +
     2c fee = 92c (edge 8c). *)
  print_opportunity (Detect.find ~mode:Exact kalshi poly);
  [%expect
    {|
    (((yes ((venue Kalshi) (market_id K1) (price 40000000)))
      (no ((venue Polymarket) (market_id P1) (price 40000000))) (cost 82000000)
      (edge 18000000) (size 10)))
    |}]
;;

let%expect_test "venue-agnostic: a pair within one venue prices with that \
                 venue's fees on both legs"
  =
  let left =
    leg
      Polymarket
      "P1"
      (book ~yes_ask:45 ~yes_size:20 ~no_ask:60 ~no_size:20)
  in
  let right =
    leg
      Polymarket
      "P2"
      (book ~yes_ask:52 ~yes_size:20 ~no_ask:48 ~no_size:20)
  in
  (* No Kalshi leg anywhere: 45c + 48c = 93c, no fee on either side. *)
  print_opportunity (Detect.find ~mode:Exact left right);
  [%expect
    {|
    (((yes ((venue Polymarket) (market_id P1) (price 45000000)))
      (no ((venue Polymarket) (market_id P2) (price 48000000))) (cost 93000000)
      (edge 7000000) (size 20)))
    |}]
;;

let%expect_test "Reckless reports fee-eaten, sizeless edges that Exact \
                 rejects — which is exactly why it must not trade"
  =
  (* 49c + 50c = 99c before fees, but the Kalshi fee is 2c (real cost 101c)
     and the YES book is empty. *)
  let kalshi =
    leg Kalshi "K1" (book ~yes_ask:49 ~yes_size:0 ~no_ask:60 ~no_size:50)
  in
  let poly =
    leg
      Polymarket
      "P1"
      (book ~yes_ask:52 ~yes_size:30 ~no_ask:50 ~no_size:40)
  in
  print_opportunity (Detect.find ~mode:Exact kalshi poly);
  print_opportunity (Detect.find ~mode:Reckless kalshi poly);
  [%expect
    {|
    ()
    (((yes ((venue Kalshi) (market_id K1) (price 49000000)))
      (no ((venue Polymarket) (market_id P1) (price 50000000))) (cost 99000000)
      (edge 1000000) (size 0)))
    |}]
;;
