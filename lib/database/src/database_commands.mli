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

(** How many still-open markets are stored ([close_time >= t]). *)
val count_market_stubs_after : (int64, int, [ `One ]) Caqti_request.t

(** How many already-closed markets are stored ([close_time < t]). *)
val count_market_stubs_before : (int64, int, [ `One ]) Caqti_request.t

(** Removes one row by primary key — how the seed purges markets to make room
    for fresh ones. *)
val delete_market_stub : (Types.Market_id.t, unit, [ `Zero ]) Caqti_request.t

(** Like {!list_market_stubs_before} but oldest [close_time] first, so the
    seed can purge the longest-closed markets. [close_time] is a BIGINT and
    sorts in SQL; volume does not (it is stored as a sexp) — rank by volume
    in OCaml instead. *)
val list_oldest_market_stubs_before
  : ( int64 * int
      , Types.Market_stub.t
(** Creates the [pair_proposals] table if it does not already exist. *)
val create_pair_proposal_table : (unit, unit, [ `Zero ]) Caqti_request.t

(** Inserts a proposal; a pair that already exists (in any status) is left
    untouched, so re-proposing never resets a human decision. *)
val insert_pair_proposal
  : (Types.Pair_proposal.t, unit, [ `Zero ]) Caqti_request.t

val set_pair_status
  : ( Types.Pair_proposal.Status.t * Types.Market_id.t * Types.Market_id.t
      , unit
      , [ `Zero ] )
      Caqti_request.t

(** Best score first. *)
val list_pair_proposals_by_status
  : ( Types.Pair_proposal.Status.t
      , Types.Pair_proposal.t
      , [ `Many | `One | `Zero ] )
      Caqti_request.t
