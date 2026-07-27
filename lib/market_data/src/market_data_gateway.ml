open! Core
open! Async
open Types

let fetch_l1_market_data ~(venue : Venue.t) ~closed ~limit =
  let%bind.Deferred.Or_error payload =
    match (venue : Venue.t) with
    | Polymarket -> Fetch_current.fetch_polymarket_markets ~closed ()
    | Kalshi ->
      let status = if closed then "closed" else "open" in
      Fetch_current.fetch_kalshi_markets ~status ()
  in
  Deferred.Or_error.return
    (Exchange_parser.parse_data ~limit ~venue payload.body)
;;
