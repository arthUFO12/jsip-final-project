(** High-level entry point combining {!Fetch_live} and {!Live_data_parser}:
    one call to get parsed L1 records for a venue. *)

open! Core
open! Async
open Types

(** [limit] caps the number of parsed markets. [events_limit] widens the
    upstream Kalshi event-listing request (its default of 100 events can
    supply fewer markets than [limit] asks for); Polymarket ignores it — its
    listing is already counted in markets. *)
val fetch_l1_market_data
  :  ?events_limit:int
  -> venue:Venue.t
  -> closed:bool
  -> limit:int
  -> unit
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
