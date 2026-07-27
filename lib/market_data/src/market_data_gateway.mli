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
