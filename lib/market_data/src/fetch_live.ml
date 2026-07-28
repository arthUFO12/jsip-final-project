open! Core
open Async
open Types

let fetch_markets_helper ~base_uri ~params ~venue =
  let modified_uri = Uri.add_query_params' base_uri params in
  let%bind state, body = Cohttp_async.Client.get modified_uri in
  match Cohttp.Response.status state with
  | #Cohttp.Code.success_status ->
    let%map data = Cohttp_async.Body.to_string body in
    Ok (Raw_payload.create ~venue ~body:data)
  | status ->
    Deferred.Or_error.error_s
      [%message
        "http request failed"
          (Cohttp.Code.string_of_status status : string)
          (venue : Venue.t)]
;;

let fetch_kalshi_markets ?(limit = 100) ?(status = "open") () =
  let base_uri =
    Uri.of_string "https://external-api.kalshi.com/trade-api/v2/events"
  in
  let params =
    [ "with_nested_markets", "true"
    ; "status", status
    ; "limit", Int.to_string limit
    ]
  in
  fetch_markets_helper ~base_uri ~params ~venue:Venue.Kalshi
;;

let fetch_polymarket_markets
  ?(closed = false)
  ?(limit = 500)
  ?(offset = 0)
  ()
  =
  let base_uri = Uri.of_string "https://gamma-api.polymarket.com/markets" in
  let params =
    [ "closed", Bool.to_string closed
    ; "limit", Int.to_string limit
    ; "offset", Int.to_string offset
    ; "include_tag", "true"
    ]
  in
  fetch_markets_helper ~base_uri ~params ~venue:Venue.Polymarket
;;
