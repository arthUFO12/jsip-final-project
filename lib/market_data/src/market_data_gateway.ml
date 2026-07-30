open! Core
open! Async
open Types

let fetch_l1_market_data ~(venue : Venue.t) ~closed ~limit =
  let%bind.Deferred.Or_error payload =
    match (venue : Venue.t) with
    | Polymarket -> Fetch_live.fetch_polymarket_markets ~closed ()
    | Kalshi ->
      let status = if closed then "closed" else "open" in
      Fetch_live.fetch_kalshi_markets ~status ()
  in
  Deferred.return (Live_data_parser.parse_data ~limit ~venue payload.body)
;;

let fetch_one_ticker_series
  (market_stub : Market_stub.t)
  ~start
  ~finish
  ~interval
  =
  match market_stub.venue with
  | Kalshi ->
    let%bind.Deferred.Or_error response_body =
      Fetch_time_series.fetch_kalshi_data
        market_stub
        ~start
        ~finish
        ~interval
    in
    Deferred.Or_error.return
      (Time_series_parser.parse_kalshi_time_series response_body)
  | Polymarket ->
    let%bind.Deferred.Or_error response_body =
      Fetch_time_series.fetch_polymarket_data
        market_stub
        ~start
        ~finish
        ~interval
    in
    Deferred.Or_error.return
      (Time_series_parser.parse_polymarket_time_series response_body)
;;

let fetch_many_ticker_series
  (market_stubs : Market_stub.t list)
  ~start
  ~finish
  ~interval
  : (Market_stub.t * Time_series.t) list Deferred.Or_error.t
  =
  let%bind.Deferred.Or_error time_series =
    List.map market_stubs ~f:(fun stub ->
      let%bind () = Clock.after (Time_float.Span.of_sec 0.1) in
      fetch_one_ticker_series stub ~start ~finish ~interval)
    |> Deferred.Or_error.all
  in
  Deferred.Or_error.return (List.zip_exn market_stubs time_series)
;;
