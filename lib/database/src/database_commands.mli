(** SQL requests used by {!Database_gateway} to manage the arbiter database. *)

open! Core

(** Creates the [market_stubs] table if it does not already exist. *)
val create_market_stub_table : (unit, unit, [ `Zero ]) Caqti_request.t

val create_config_table : (unit, unit, [ `Zero ]) Caqti_request.t

val find_market_stub
  : ( Types.Market_id.t
      , Types.Market_stub.t
      , [ `One | `Zero ] )
      Caqti_request.t

(** Upserts on the [market_id] primary key ([INSERT OR REPLACE]). *)
val insert_market_stub
  : (Types.Market_stub.t, unit, [ `Zero ]) Caqti_request.t

(** Stubs with [close_time >= t], i.e. still-open markets. *)
val list_market_stubs_after
  : ( int64 * int
      , Types.Market_stub.t
      , [ `Many | `One | `Zero ] )
      Caqti_request.t

(** Like {!find_market_stub} but keyed on the (Kalshi-unique) slug, which is
    how the web client names markets. *)
val find_market_stub_by_slug
  : (string, Types.Market_stub.t, [ `One | `Zero ]) Caqti_request.t

(** Empties the [market_stubs] table, so a fresh seed fully replaces the
    previous one instead of accumulating stale markets. *)
val delete_market_stubs : (unit, unit, [ `Zero ]) Caqti_request.t

(** Stubs with [close_time < t], i.e. already-closed (historical) markets. *)
val list_market_stubs_before
  : ( int64 * int
      , Types.Market_stub.t
      , [ `Many | `One | `Zero ] )
      Caqti_request.t
