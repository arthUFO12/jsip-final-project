open! Core
open Async
open Types

(** Network transport for venue REST endpoints. Returns raw, unparsed
    payloads tagged with venue and ingress timestamp — parsing is the
    caller's concern via [Exchange_parser]. *)

(** Fetch the current active-market listing for [venue].

    Kalshi: GET /trade-api/v2/markets?mve_filter=exclude&limit=.. Polymarket:
    GET /markets?active=true&limit=..

    Errors on network failure, non-2xx status, or timeout. *)
val fetch_markets
  :  ?limit:int (* default 100 *)
  -> venue:Venue.t
  -> unit
  -> Raw_payload.t Deferred.Or_error.t
