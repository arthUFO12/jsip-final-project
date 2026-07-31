(** High-level entry point combining {!Fetch_live} and {!Live_data_parser}:
    one call to get parsed L1 records for a venue. *)

open! Core
open! Async
open Types

val fetch_l1_market_data
  :  venue:Venue.t
  -> closed:bool
  -> limit:int
  -> L1_market_metadata.t list Deferred.Or_error.t

(** One market's live order book — best ask each side with real depth — via
    {!Fetch_books} and {!Book_parser}. Venue is taken from the stub;
    Polymarket stubs must carry a [clob_token_id]. Errors (network, empty
    book side, missing token) are returned, never raised, so callers sweeping
    many markets can skip the bad ones. *)
val fetch_book : Market_stub.t -> Binary_book.t Deferred.Or_error.t

val fetch_one_ticker_series
  :  Market_stub.t
  -> start:Time_ns.t
  -> finish:Time_ns.t
  -> interval:Time_series.Interval.t
  -> Time_series.t Deferred.Or_error.t

val fetch_many_ticker_series
  :  Market_stub.t list
  -> start:Time_ns.t
  -> finish:Time_ns.t
  -> interval:Time_series.Interval.t
  -> (Market_stub.t * Time_series.t) list Deferred.Or_error.t

(** Polymarket markets whose events match a free-text query, via
    {!Fetch_live.fetch_polymarket_search} and
    {!Live_data_parser.parse_polymarket_search}. The sweep's server-side
    funnel: recall is the venue's search engine's, so a miss here says
    nothing definitive — only a full listing sweep bounds missed pairs. *)
val search_polymarket
  :  query:string
  -> L1_market_metadata.t list Deferred.Or_error.t

(** Every open market the venue will list, paged to exhaustion (Kalshi by
    cursor, Polymarket by offset). Slow — tens of requests — and meant for
    occasional backstop sweeps, not the hot path. A runaway listing stops at
    an internal page cap with a note on stderr, never silently. *)
val fetch_all_l1_market_data
  :  venue:Venue.t
  -> L1_market_metadata.t list Deferred.Or_error.t
