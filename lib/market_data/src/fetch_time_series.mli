(** Network transport for venue price-history endpoints. Returns the raw
    response body; decoding is the caller's concern via
    {!Time_series_parser}. *)

open! Core
open! Async
open Types

(** Refresh the cached Kalshi historical cutoff (the settle time before which
    markets live on the historical endpoint rather than the live one). Must
    complete successfully at least once before calling {!fetch_kalshi_data},
    which raises otherwise. *)
val update_kalshi_historical_cutoff : unit -> unit Deferred.Or_error.t

(** Fetch candlesticks for a Kalshi market between [start] and [finish],
    routing to the live or historical endpoint by comparing the stub's close
    time against the cutoff. Raises if the stub lacks [series_ticker], or if
    {!update_kalshi_historical_cutoff} has never succeeded. *)
val fetch_kalshi_data
  :  Market_stub.t
  -> start:Time_ns.t
  -> finish:Time_ns.t
  -> interval:Time_series.Interval.t
  -> string Deferred.Or_error.t

(** Fetch yes-price history for a Polymarket market from clob
    /prices-history. Raises if the stub lacks [clob_token_id]. *)
val fetch_polymarket_data
  :  Market_stub.t
  -> start:Time_ns.t
  -> finish:Time_ns.t
  -> interval:Time_series.Interval.t
  -> string Deferred.Or_error.t
