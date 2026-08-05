(** The database's public face: connect once, then run typed queries against
    the connection pool. Statements themselves live in {!Database_commands};
    this module supplies the pool, the [Deferred.Or_error] boundary, and the
    current time.

    Call {!init_database} (then {!create_market_stub_table}) before anything
    else — every other function fails with a connection error until the pool
    exists. *)

open! Core
open Async
open Types

(** [init_database file_name] opens (creating if needed) the sqlite database
    [file_name] under the project root and stores the connection pool for
    every later call. *)
val init_database : string -> unit Or_error.t

val create_market_stub_table : unit -> unit Deferred.Or_error.t

(** Upserts by [market_id]: re-inserting a stub replaces the stored row, so
    repeated seeding is idempotent. *)
val insert_market_stub : Market_stub.t -> unit Deferred.Or_error.t

(** {!insert_market_stub} over a whole fetch, stopping at the first error. *)
val insert_market_stubs : Market_stub.t list -> unit Deferred.Or_error.t

(** Empties the table. The seed no longer does this on startup (it purges and
    tops up incrementally instead); kept for tests and manual resets. *)
val clear_market_stubs : unit -> unit Deferred.Or_error.t

(** Deletes the given rows by [market_id], stopping at the first error — how
    the seed evicts markets to make room for fresh ones. *)
val delete_market_stubs_by_ids : Market_id.t list -> unit Deferred.Or_error.t

val find_market_stub
  :  Market_id.t
  -> Market_stub.t option Deferred.Or_error.t

(** Lookup by slug — how the web client names markets. *)
val find_market_stub_by_slug
  :  Slug.t
  -> Market_stub.t option Deferred.Or_error.t

(** Stubs whose market is still open ([close_time] in the future), at most
    [limit], in no particular order — rank in OCaml (e.g. by
    {!Volume.to_float}) after loading. *)
val list_current_market_stubs : int -> Market_stub.t list Deferred.Or_error.t

(** Stubs whose market has already closed — the historical half of the seed.
    Same ordering caveat as {!list_current_market_stubs}. *)
val list_historical_market_stubs
  :  int
  -> Market_stub.t list Deferred.Or_error.t

(** Like {!list_historical_market_stubs} but oldest [close_time] first — the
    purge order for the historical half. *)
val list_oldest_historical_market_stubs
  :  int
  -> Market_stub.t list Deferred.Or_error.t

(** How many still-open markets are stored — the seed's "is the database
    full" check for the current half. *)
val count_current_market_stubs : unit -> int Deferred.Or_error.t

(** Companion count for the already-closed half. *)
val count_historical_market_stubs : unit -> int Deferred.Or_error.t

module For_testing : sig
  (** The listing and counting queries with the "now" boundary injected, so
      tests are independent of the wall clock. *)

  val list_current_market_stubs
    :  int
    -> time:Time_ns.t
    -> Market_stub.t list Deferred.Or_error.t

  val list_historical_market_stubs
    :  int
    -> time:Time_ns.t
    -> Market_stub.t list Deferred.Or_error.t

  val count_current_market_stubs : time:Time_ns.t -> int Deferred.Or_error.t

  val count_historical_market_stubs
    :  time:Time_ns.t
    -> int Deferred.Or_error.t

  val list_oldest_historical_market_stubs
    :  int
    -> time:Time_ns.t
    -> Market_stub.t list Deferred.Or_error.t
end

(*_ The pair-proposal store: the review gate described in
    [lib/types/src/pairs.mli]. *)

val create_pair_proposal_table : unit -> unit Deferred.Or_error.t

(** Records a proposal. A pair already present — whatever its status — is
    left untouched, so sweeps can re-propose freely without resurrecting
    rejected pairs or resetting approvals. *)
val propose_pair : Pair_proposal.t -> unit Deferred.Or_error.t

(** The human decision. [left]/[right] must be in the canonical order
    {!Pair_proposal.create} produces. *)
val set_pair_status
  :  left:Market_id.t
  -> right:Market_id.t
  -> Pair_proposal.Status.t
  -> unit Deferred.Or_error.t

(** Best score first. The bot reads [Approved]; review reads [Proposed]. *)
val list_pair_proposals_by_status
  :  Pair_proposal.Status.t
  -> Pair_proposal.t list Deferred.Or_error.t

(** {!set_pair_status} plus the adjudicator's explanation in one write — the
    LLM review path, where the rationale is the audit trail. *)
val set_pair_verdict
  :  left:Market_id.t
  -> right:Market_id.t
  -> Pair_proposal.Status.t
  -> string option
  -> unit Deferred.Or_error.t

(*_ The pair store's leg snapshots. [market_stubs] is a rotating catalog
    whose seed purges rows at will, so a proposal's legs must live somewhere
    the purge cannot reach — else an approved pair silently stops pricing
    when its market rotates out. *)

val create_pair_stub_table : unit -> unit Deferred.Or_error.t

(** Upserts by [market_id]: a sweep that rediscovers a pair refreshes its leg
    snapshots. *)
val insert_pair_stub : Market_stub.t -> unit Deferred.Or_error.t

(** The snapshot for one leg, falling back to the catalog
    ({!find_market_stub}) for pairs proposed before the snapshot table
    existed. *)
val find_pair_stub : Market_id.t -> Market_stub.t option Deferred.Or_error.t

(** Copies every catalog stub a proposal references into the snapshot table,
    keeping snapshots already taken. Call after {!create_pair_stub_table} and
    {e before} any catalog purge, so legs still in the catalog are rescued
    rather than evicted. *)
val backfill_pair_stubs : unit -> unit Deferred.Or_error.t

(*_ The "for fun" arbitrage wallet: every tradable edge a scan finds is
    booked at the profit acting on it would have locked in; the user marks
    the ones they really traded. See {!Types.Wallet_entry}. *)

val create_arb_wallet_table : unit -> unit Deferred.Or_error.t

(** Upserts by [pair_key]; rows already marked acted are frozen. *)
val upsert_wallet_entry : Wallet_entry.t -> unit Deferred.Or_error.t

(** Freezes the row as really-traded, banking its current [dollars] into
    [acted_dollars]. No-op if already acted or unknown. *)
val mark_wallet_acted : pair_key:string -> unit Deferred.Or_error.t

(** Best would-have-made first. *)
val list_wallet_entries : unit -> Wallet_entry.t list Deferred.Or_error.t

(*_ The live-trading audit trail: append-only — nothing can update or delete
    a row. See {!Types.Trade_log_entry}. *)

(** The append-only edge-sighting history behind real arb PnL-over-time: one
    row per tradable edge per scan; never updated or deleted. *)
val create_arb_observation_table : unit -> unit Deferred.Or_error.t

val append_arb_observation : Arb_observation.t -> unit Deferred.Or_error.t

(** All sightings, oldest first. *)
val list_arb_observations
  :  unit
  -> Arb_observation.t list Deferred.Or_error.t

val create_trade_log_table : unit -> unit Deferred.Or_error.t
val append_trade_log : Trade_log_entry.t -> unit Deferred.Or_error.t

(** Newest first, at most [limit] rows. *)
val list_trade_log : int -> Trade_log_entry.t list Deferred.Or_error.t

(** Sum of accepted-placement notionals at or after [time] — what the per-day
    live spending cap charges against. *)
val sum_trade_dollars_since : Time_ns.t -> float Deferred.Or_error.t

(** Like {!mark_wallet_acted} but with the dollars supplied (assisted flow:
    self-reported manual leg + verified hedge) and the summary visibly marked
    [self-reported]. *)
val mark_wallet_acted_assisted
  :  pair_key:string
  -> dollars:float
  -> unit Deferred.Or_error.t
