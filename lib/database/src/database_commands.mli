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
      , [ `Many | `One | `Zero ] )
      Caqti_request.t

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

(** {!set_pair_status} plus the adjudicator's explanation in one write. *)
val set_pair_verdict
  : ( Types.Pair_proposal.Status.t
      * string option
      * Types.Market_id.t
      * Types.Market_id.t
      , unit
      , [ `Zero ] )
      Caqti_request.t

(** Creates the [pair_stubs] table — the pair store's own snapshot of each
    proposal's legs, immune to the catalog's rotation — if it does not
    already exist. Same columns as [market_stubs]. *)
val create_pair_stub_table : (unit, unit, [ `Zero ]) Caqti_request.t

(** Upserts a leg snapshot by [market_id]: a sweep that sees the market again
    refreshes it. *)
val insert_pair_stub : (Types.Market_stub.t, unit, [ `Zero ]) Caqti_request.t

val find_pair_stub
  : ( Types.Market_id.t
      , Types.Market_stub.t
      , [ `One | `Zero ] )
      Caqti_request.t

(** Copies every catalog row a proposal references into [pair_stubs], keeping
    snapshots already taken — the one-time rescue for stores from before the
    snapshot table existed. Run it before the catalog purge. *)
val backfill_pair_stubs : (unit, unit, [ `Zero ]) Caqti_request.t

(** Creates the [arb_wallet] table — the "for fun" paper-trading score, one
    row per pair ever seen tradable — if it does not already exist. *)
val create_arb_wallet_table : (unit, unit, [ `Zero ]) Caqti_request.t

(** Upserts by [pair_key]; rows already marked acted are frozen and never
    updated again. Inserts arrive with [acted = false]. *)
val upsert_wallet_entry
  : (Types.Wallet_entry.t, unit, [ `Zero ]) Caqti_request.t

(** Freezes a row as really-traded, banking its current [dollars] into
    [acted_dollars]. A second call is a no-op. *)
val mark_wallet_acted : (string, unit, [ `Zero ]) Caqti_request.t

(** Best would-have-made first. *)
val list_wallet_entries
  : (unit, Types.Wallet_entry.t, [ `Many | `One | `Zero ]) Caqti_request.t

(** Creates the append-only [trade_log] audit table if it does not already
    exist. No update/delete statements exist for it, by design. *)
val create_arb_observation_table : (unit, unit, [ `Zero ]) Caqti_request.t

val append_arb_observation
  : (Types.Arb_observation.t, unit, [ `Zero ]) Caqti_request.t

val list_arb_observations
  : (unit, Types.Arb_observation.t, [ `Many | `One | `Zero ]) Caqti_request.t

val create_trade_log_table : (unit, unit, [ `Zero ]) Caqti_request.t

val append_trade_log
  : (Types.Trade_log_entry.t, unit, [ `Zero ]) Caqti_request.t

(** Newest first, at most N rows. *)
val list_trade_log
  : (int, Types.Trade_log_entry.t, [ `Many | `One | `Zero ]) Caqti_request.t

(** Sum of accepted-placement notionals since the cutoff (int64 time) — the
    per-day live spending cap's ledger. *)
val sum_trade_dollars_since : (int64, float, [ `One ]) Caqti_request.t

(** Freezes a row as really-traded at the given dollars (assisted flow:
    manual leg self-reported + venue-verified hedge), prefixing the summary
    with a visible [self-reported] marker. No-op if already acted. *)
val mark_wallet_acted_assisted
  : (float * string, unit, [ `Zero ]) Caqti_request.t
