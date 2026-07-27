(** High-level entry point combining {!Fetch_live} and
    {!Live_data_parser}: one call to get parsed L1 records for a venue. *)

open! Core
open! Async
open Types

val fetch_l1_market_data
  :  venue:Venue.t
  -> closed:bool
  -> limit:int
  -> L1_market_metadata.t list Deferred.Or_error.t
