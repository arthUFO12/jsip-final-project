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

let fetch_kalshi_markets ?(limit = 100) ?(status = "open") ?cursor () =
  let base_uri =
    Uri.of_string "https://external-api.kalshi.com/trade-api/v2/events"
  in
  let params =
    [ "with_nested_markets", "true"
    ; "status", status
    ; "limit", Int.to_string limit
    ]
    @ match cursor with None -> [] | Some cursor -> [ "cursor", cursor ]
  in
  fetch_markets_helper ~base_uri ~params ~venue:Venue.Kalshi
;;

let fetch_polymarket_markets
  ?(closed = false)
  ?(limit = 500)
  ?(offset = 0)
  ?(order_by_24h_volume = false)
  ()
  =
  let base_uri = Uri.of_string "https://gamma-api.polymarket.com/markets" in
  let params =
    [ "closed", Bool.to_string closed
    ; "limit", Int.to_string limit
    ; "offset", Int.to_string offset
    ; "include_tag", "true"
    ]
    @
    if order_by_24h_volume
    then [ "order", "volume24hr"; "ascending", "false" ]
    else []
  in
  fetch_markets_helper ~base_uri ~params ~venue:Venue.Polymarket
;;

let fetch_polymarket_search ~query ?(limit_per_type = 20) () =
  let base_uri =
    Uri.of_string "https://gamma-api.polymarket.com/public-search"
  in
  let params =
    [ "q", query
    ; "limit_per_type", Int.to_string limit_per_type
    ; "events_status", "active"
    ]
  in
  fetch_markets_helper ~base_uri ~params ~venue:Venue.Polymarket
;;
