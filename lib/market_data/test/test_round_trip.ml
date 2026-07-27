open! Core
open Async
open Market_data
open Types

let markets_fetched = 5

(* Live round trip: hit the real venue endpoint with [Fetch_live], then feed
   the raw body through [Live_data_parser]. These tests use the network, so
   unlike the fixture tests their input is different on every run: the
   summary below must print properties that hold for any healthy response,
   never market contents. *)

let print_summary ~(venue : Venue.t) (markets : L1_market_metadata.t list) =
  print_endline [%string "Number of markets %{(List.length markets)#Int}"];
  print_endline
    [%string
      "All markets open: %{(List.for_all markets ~f:(fun market -> \
       market.active))#Bool}"];
  if match venue with Kalshi -> true | Polymarket -> false
  then
    print_endline
      [%string
        "All kalshi markets have series ticker: %{(List.for_all markets \
         ~f:(fun market -> Option.is_some market.series_ticker))#Bool}"]
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
       Live_data_parser.parse_data
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
      Fetch_live.fetch_kalshi_markets ~limit:2 ())
  in
  [%expect
    {|
    Number of markets 5
    All markets open: true
    All kalshi markets have series ticker: true
    |}]
;;

let%expect_test "polymarket fetch -> parse round trip" =
  let%map () =
    round_trip ~venue:Venue.Polymarket ~fetch:(fun () ->
      Fetch_live.fetch_polymarket_markets ~limit:10 ())
  in
  [%expect {|
    Number of markets 5
    All markets open: true
    |}]
;;

(* Time-series round trip: pick a live market via the L1 pipeline, project it
   to a [Market_stub.t], then fetch and parse its recent price history. Same
   rule as above: print invariants, never point contents. *)

let print_time_series_summary (points : Time_series.Point.t list) =
  let has_points = not (List.is_empty points) in
  let all_prices_in_band =
    List.for_all points ~f:(fun (point : Time_series.Point.t) ->
      Price.(point.yes_price >= zero) && Price.(point.yes_price <= one))
  in
  let times_nondecreasing =
    List.is_sorted
      (List.map points ~f:(fun (point : Time_series.Point.t) -> point.time))
      ~compare:Time_ns.compare
  in
  print_endline [%string "Has points: %{has_points#Bool}"];
  print_endline
    [%string "All prices within $0..$1: %{all_prices_in_band#Bool}"];
  print_endline [%string "Times nondecreasing: %{times_nondecreasing#Bool}"]
;;

let time_series_round_trip
  ~(venue : Venue.t)
  ~usable
  ~fetch_markets
  ~fetch_series
  ~parse_series
  =
  match%bind fetch_markets () with
  | Error e ->
    return (print_s [%message "market fetch failed" (e : Error.t)])
  | Ok payload ->
    (match Live_data_parser.parse_data ~venue (Raw_payload.body payload) with
     | Error e -> return (print_s [%message "parse failed" (e : Error.t)])
     | Ok markets ->
       (match List.find markets ~f:usable with
        | None -> return (print_endline "no usable market found")
        | Some market ->
          let stub = L1_market_metadata.to_market_stub market in
          let finish = Time_ns.now () in
          let start = Time_ns.sub finish (Time_ns.Span.of_hr 6.) in
          (match%map
             fetch_series
               stub
               ~start
               ~finish
               ~interval:Time_series.Interval.Hour
           with
           | Error e ->
             print_s [%message "time series fetch failed" (e : Error.t)]
           | Ok body -> print_time_series_summary (parse_series body))))
;;

let%expect_test "kalshi time series fetch -> parse round trip" =
  let%bind () =
    match%map Fetch_time_series.update_kalshi_historical_cutoff () with
    | Error e -> print_s [%message "cutoff fetch failed" (e : Error.t)]
    | Ok () -> ()
  in
  let%map () =
    time_series_round_trip
      ~venue:Venue.Kalshi
      ~usable:(fun (market : L1_market_metadata.t) ->
        Option.is_some market.series_ticker
        && Option.is_some market.close_time)
      ~fetch_markets:(fun () -> Fetch_live.fetch_kalshi_markets ~limit:1 ())
      ~fetch_series:Fetch_time_series.fetch_kalshi_data
      ~parse_series:Time_series_parser.parse_kalshi_time_series
  in
  [%expect
    {|
    Has points: true
    All prices within $0..$1: true
    Times nondecreasing: true
    |}]
;;

let%expect_test "polymarket time series fetch -> parse round trip" =
  let%map () =
    time_series_round_trip
      ~venue:Venue.Polymarket
      ~usable:(fun (market : L1_market_metadata.t) ->
        Option.is_some market.clob_token_id)
      ~fetch_markets:(fun () ->
        Fetch_live.fetch_polymarket_markets ~limit:10 ())
      ~fetch_series:Fetch_time_series.fetch_polymarket_data
      ~parse_series:Time_series_parser.parse_polymarket_time_series
  in
  [%expect
    {|
    Has points: true
    All prices within $0..$1: true
    Times nondecreasing: true
    |}]
;;
