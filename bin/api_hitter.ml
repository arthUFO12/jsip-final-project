open! Core
open Async
open Types

let fetch_markets_helper ~base_uri ~params ~venue =
  let modified_uri = Uri.add_query_params' base_uri params in
  let%bind _, body = Cohttp_async.Client.get modified_uri in
  let%map data = Cohttp_async.Body.to_string body in
  Raw_payload.create ~venue ~body:data
;;

(* later add a status type for errors and typos to be compile errors when
   users begin calling *)
(* fetches active market data from kalshi *)
let fetch_kalshi_markets
  ?(limit = 100)
  ?(mve_filter = "exclude")
  ?(status = "open")
  ()
  =
  let base_uri =
    Uri.of_string "https://external-api.kalshi.com/trade-api/v2/markets"
  in
  let params =
    [ "limit", Int.to_string limit
    ; "mve_filter", mve_filter
    ; "status", status
    ]
  in
  fetch_markets_helper ~base_uri ~params ~venue:Venue.Kalshi
;;

(* fetches active market data from polymarket *)
let fetch_polymarket_markets ?(closed = false) ?(limit = 100) () =
  let base_uri = Uri.of_string "https://gamma-api.polymarket.com/markets" in
  let params =
    [ "closed", Bool.to_string closed; "limit", Int.to_string limit ]
  in
  fetch_markets_helper ~base_uri ~params ~venue:Venue.Polymarket
;;
