open! Core
open Async
open Types

(** Network transport for venue REST endpoints. Returns raw, unparsed
    payloads tagged with venue and ingress timestamp — parsing is the
    caller's concern via {!Live_data_parser}. *)

(** Fetch the event listing (with nested markets) from Kalshi. GET
    /trade-api/v2/events?with_nested_markets=true&status=..&limit=..

    [limit] caps the number of {e events} returned (each event nests one or
    more markets); to cap the number of parsed markets, pass [?limit] to
    {!Live_data_parser.parse_data}. [status] selects the market lifecycle
    stage: ["open"] (default), ["closed"], or ["settled"]. *)
val fetch_kalshi_markets
  :  ?limit:int (* default 100 *)
  -> ?status:string (* default "open" *)
  -> ?cursor:
       string
       (* resume a paged listing; see {!Live_data_parser.parse_kalshi_cursor} *)
  -> unit
  -> Raw_payload.t Deferred.Or_error.t

(** Fetch the market listing from Polymarket's gamma API. GET
    /markets?closed=..&limit=..&offset=..&include_tag=true

    [closed] selects resolved markets instead of live ones. [limit] caps the
    number of markets, but gamma silently serves at most 100 per page
    regardless; [offset] pages through the listing and is refused (422) past
    2500 — the venue's public listing bound. [order_by_24h_volume] serves the
    most-traded markets first, which is how a paged sweep should spend that
    bound. Tags are always included — {!Live_data_parser} needs them to
    derive the category. *)
val fetch_polymarket_markets
  :  ?closed:bool (* default false *)
  -> ?limit:int (* default 500; gamma caps a page at 100 *)
  -> ?offset:int (* default 0 *)
  -> ?order_by_24h_volume:bool (* default false *)
  -> unit
  -> Raw_payload.t Deferred.Or_error.t

(** Text search over Polymarket's live listings. GET
    /public-search?q=..&limit_per_type=..&events_status=active

    [query] is free text; [limit_per_type] caps the number of {e events}
    returned (each nests one or more markets). Only active events are
    searched. Parse the payload with
    {!Live_data_parser.parse_polymarket_search}. *)
val fetch_polymarket_search
  :  query:string
  -> ?limit_per_type:int (* default 20 *)
  -> unit
  -> Raw_payload.t Deferred.Or_error.t
