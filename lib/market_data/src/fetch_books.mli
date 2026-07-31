open! Core
open Async
open Types

(** Network transport for venue order-book endpoints. Returns raw, unparsed
    payloads tagged with venue and ingress timestamp — parsing is the
    caller's concern via {!Book_parser}. One request fetches one market's
    book; callers batching over many markets own their own pacing. *)

(** Fetch one Kalshi market's order book. GET
    /trade-api/v2/markets/[{ticker}]/orderbook

    [ticker] is the market ticker ({!Market_id.t} rendered as a string, e.g.
    ["KXBTC-26JUL22-T118000"]). *)
val fetch_kalshi_book
  :  ticker:string
  -> unit
  -> Raw_payload.t Deferred.Or_error.t

(** Fetch one Polymarket token's order book from the CLOB. GET
    https://clob.polymarket.com/book?token_id=..

    [token_id] is the market's YES-outcome CLOB token ({!Market_stub.t}'s
    [clob_token_id]). *)
val fetch_polymarket_book
  :  token_id:string
  -> unit
  -> Raw_payload.t Deferred.Or_error.t
