open! Core
open Async
open Types

let fetch_book_helper ~uri ~venue =
  let%bind state, body = Cohttp_async.Client.get uri in
  match Cohttp.Response.status state with
  | #Cohttp.Code.success_status ->
    let%map data = Cohttp_async.Body.to_string body in
    Ok (Raw_payload.create ~venue ~body:data)
  | status ->
    Deferred.Or_error.error_s
      [%message
        "http request failed"
          (Cohttp.Code.string_of_status status : string)
          (venue : Venue.t)
          (Uri.to_string uri : string)]
;;

(* The primary host, not external-api: the mirror {!Fetch_live} lists from
   serves null orderbooks, while this host serves real depth (in the
   [orderbook_fp] shape). Tickers are global, so listing from one host and
   fetching books from the other is fine. *)
let fetch_kalshi_book ~ticker () =
  let uri =
    Uri.of_string
      [%string
        "https://api.elections.kalshi.com/trade-api/v2/markets/%{ticker}/orderbook"]
  in
  fetch_book_helper ~uri ~venue:Venue.Kalshi
;;

let fetch_polymarket_book ~token_id () =
  let uri =
    Uri.add_query_param'
      (Uri.of_string "https://clob.polymarket.com/book")
      ("token_id", token_id)
  in
  fetch_book_helper ~uri ~venue:Venue.Polymarket
;;
