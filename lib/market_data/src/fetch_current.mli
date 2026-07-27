open! Core
open Async
open Types

(** Network transport for venue REST endpoints. Returns raw, unparsed
    payloads tagged with venue and ingress timestamp — parsing is the
    caller's concern via {!Exchange_parser}. *)

(** Fetch the event listing (with nested markets) from Kalshi. GET
    /trade-api/v2/events?with_nested_markets=true&status=..&limit=..

    [limit] caps the number of {e events} returned (each event nests one or
    more markets); to cap the number of parsed markets, pass [?limit] to
    {!Exchange_parser.parse_data}. [status] selects the market lifecycle
    stage: ["open"] (default), ["closed"], or ["settled"]. *)
val fetch_kalshi_markets
  :  ?limit:int (* default 30 *)
  -> ?status:string (* default "open" *)
  -> unit
  -> Raw_payload.t Deferred.Or_error.t

(** Fetch the market listing from Polymarket's gamma API. GET
    /markets?closed=..&limit=..&offset=..&include_tag=true

    [closed] selects resolved markets instead of live ones. [limit] caps the
    number of markets; [offset] pages through the listing. Tags are always
    included — {!Exchange_parser} needs them to derive the category. *)
val fetch_polymarket_markets
  :  ?closed:bool (* default false *)
  -> ?limit:int (* default 500 *)
  -> ?offset:int (* default 0 *)
  -> unit
  -> Raw_payload.t Deferred.Or_error.t
