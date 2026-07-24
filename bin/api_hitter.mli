open! Core
open Async
open Types

(** Network transport for venue REST endpoints. Returns raw, unparsed
    payloads tagged with venue and ingress timestamp — parsing is the
    caller's concern via [Exchange_parser]. *)

(** Fetch the current active-market listing from Kalshi. GET
    /trade-api/v2/markets?limit=.. *)
val fetch_kalshi_markets
  :  ?limit:int (* default 100 *)
  -> ?mve_filter:string
  -> ?status:string
  -> unit
  -> Raw_payload.t Deferred.Or_error.t

(** Fetch the current active-market listing from Polymarket. GET
    /markets?closed=..&limit=.. *)
val fetch_polymarket_markets
  :  ?closed:bool (* default false *)
  -> ?limit:int (* default 100 *)
  -> unit
  -> Raw_payload.t Deferred.Or_error.t
