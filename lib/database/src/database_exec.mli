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

(** Empties the table. Run before reseeding so the database holds exactly the
    latest seed rather than the union of every past one. *)
val clear_market_stubs : unit -> unit Deferred.Or_error.t

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

module For_testing : sig
  (** The listing queries with the "now" boundary injected, so tests are
      independent of the wall clock. *)

  val list_current_market_stubs
    :  int
    -> time:Time_ns.t
    -> Market_stub.t list Deferred.Or_error.t

  val list_historical_market_stubs
    :  int
    -> time:Time_ns.t
    -> Market_stub.t list Deferred.Or_error.t
end
