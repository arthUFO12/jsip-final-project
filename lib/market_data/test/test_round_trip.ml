open! Core
open Async
open Market_data
open Types


let markets_fetched = 5
;;
(* Live round trip: hit the real venue endpoint with [Fetch_current], then
   feed the raw body through [Exchange_parser]. These tests use the network,
   so unlike the fixture tests their input is different on every run: the
   summary below must print properties that hold for any healthy response,
   never market contents. *)

let print_summary ~(venue : Venue.t) (markets : L1_market_metadata.t list) =
  print_endline [%string "Number of markets %{(List.length markets)#Int}"];
  print_endline [%string "All markets open: %{(List.for_all markets ~f:(fun market -> market.active))#Bool}"];
  if match venue with Kalshi -> true | Polymarket -> false then
    print_endline [%string "All kalshi markets have series ticker: %{(List.for_all markets ~f:(fun market -> Option.is_some market.series_ticker))#Bool}"];
;;

let round_trip ~(venue : Venue.t) ~fetch =
  match%map fetch () with
  | Error e -> print_s [%message "fetch failed" (e : Error.t)]
  | Ok payload ->
    if not (Venue.equal (Raw_payload.venue payload) venue)
    then
      print_s
        [%message
          "payload tagged with wrong venue"
            (Raw_payload.venue payload : Venue.t)];
    (match
       Exchange_parser.parse_data
         ~limit:markets_fetched
         ~venue
         (Raw_payload.body payload)
     with
     | Error e -> print_s [%message "parse failed" (e : Error.t)]
     | Ok markets -> print_summary ~venue markets)
;;

let%expect_test "kalshi fetch -> parse round trip" =
  let%map () =
    round_trip ~venue:Venue.Kalshi ~fetch:(fun () ->
      Fetch_current.fetch_kalshi_markets ~limit:2 ())
  in
  [%expect {|
    Number of markets 5
    All markets open: true
    All kalshi markets have series ticker: true
    |}]
;;

let%expect_test "polymarket fetch -> parse round trip" =
  let%map () =
    round_trip ~venue:Venue.Polymarket ~fetch:(fun () ->
      Fetch_current.fetch_polymarket_markets ~limit:10 ())
  in
  [%expect {|
    Number of markets 5
    All markets open: true
    |}]
;;
