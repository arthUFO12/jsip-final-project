open! Core
open Market_data
open Types

(* Fixture: best YES bid 47c x 80, best NO bid 51c x 60. Buying YES lifts the
   NO bid (ask 49c x 60); buying NO lifts the YES bid (ask 53c x 80). *)
let%expect_test "parse kalshi orderbook fixture" =
  let body = In_channel.read_all "example_json/kalshi_orderbook.json" in
  print_s
    [%sexp (Book_parser.parse_kalshi_book body : Binary_book.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((yes_ask 49000000) (yes_ask_size 60) (no_ask 53000000) (no_ask_size 80)))
    |}]
;;

(* Fixture (current orderbook_fp shape, from a real GDP market): best YES bid
   $0.05 x 12.69, best NO bid $0.93 x 73.84. Derived: YES ask 7c x 73, NO ask
   95c x 12 — the 7c matches the venue's own quoted yes_ask. *)
let%expect_test "parse kalshi orderbook_fp fixture" =
  let body = In_channel.read_all "example_json/kalshi_orderbook_fp.json" in
  print_s
    [%sexp (Book_parser.parse_kalshi_book body : Binary_book.t Or_error.t)];
  [%expect
    {| (Ok ((yes_ask 7000000) (yes_ask_size 73) (no_ask 95000000) (no_ask_size 12))) |}]
;;

let%expect_test "kalshi: a null orderbook (no resting orders) is a clean \
                 error"
  =
  let body = {|{"orderbook": null}|} in
  print_s
    [%sexp (Book_parser.parse_kalshi_book body : Binary_book.t Or_error.t)];
  [%expect
    {| (Error ("could not parse kalshi orderbook" "orderbook is empty (null)")) |}]
;;

let%expect_test "kalshi: an empty side is an error, not a made-up quote" =
  let body = {|{"orderbook": {"yes": [[47, 80]], "no": null}}|} in
  print_s
    [%sexp (Book_parser.parse_kalshi_book body : Binary_book.t Or_error.t)];
  [%expect
    {| (Error ("could not parse kalshi orderbook" ("no resting bids" (side no)))) |}]
;;

(* Fixture: best ask 0.52 x 44.9 shares, best bid 0.50 x 125.5. YES ask is
   quoted directly (fractional size floored); NO ask = $1 - 0.50. *)
let%expect_test "parse polymarket book fixture" =
  let body = In_channel.read_all "example_json/polymarket_book.json" in
  print_s
    [%sexp
      (Book_parser.parse_polymarket_book body : Binary_book.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((yes_ask 52000000) (yes_ask_size 44) (no_ask 50000000) (no_ask_size 125)))
    |}]
;;

let%expect_test "polymarket: no resting asks is an error" =
  let body = {|{"bids": [{"price": "0.50", "size": "10"}], "asks": []}|} in
  print_s
    [%sexp
      (Book_parser.parse_polymarket_book body : Binary_book.t Or_error.t)];
  [%expect
    {| (Error ("could not parse polymarket book" ("no resting orders" (side asks)))) |}]
;;

let%expect_test "kalshi cursor extraction" =
  List.iter
    [ {|{"cursor": "CgwI5vi", "events": []}|}
    ; {|{"cursor": "", "events": []}|}
    ; {|{"events": []}|}
    ; {|not json|}
    ]
    ~f:(fun body ->
      print_s
        [%sexp (Live_data_parser.parse_kalshi_cursor body : string option)]);
  [%expect {|
    (CgwI5vi)
    ()
    ()
    ()
    |}]
;;
