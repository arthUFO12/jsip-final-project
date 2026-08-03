(** Pure policy for the rotating seed: which stored markets to purge when the
    database is full, and which fetched markets to insert to take their
    place. Kept free of IO so tests can drive it with hand-built stubs; the
    seed in [app/server] wires it to {!Database_exec}. *)

open! Core
open Types

(** The [count] lowest-volume markets, purged first. Markets with no recorded
    volume rank lowest of all — an unknown market is the least interesting
    one to keep. *)
val live_purge_victims : Market_stub.t list -> count:int -> Market_id.t list

(** Picks at most [needed] stubs from [fetched] to insert after a purge.

    The venue listing cannot be paginated, so a refetch mostly returns the
    markets already stored — including the ones just purged. Preference
    order: markets seen for the first time ([market_id] in neither
    [existing_ids] nor [purged_ids]) fill the quota first; only if the
    listing runs out of genuinely new markets are just-purged ones re-added.
    Returns the chosen stubs and how many of them are re-adds, so the seed
    can log an honest rotation count. *)
val select_new
  :  fetched:Market_stub.t list
  -> existing_ids:Market_id.Set.t
  -> purged_ids:Market_id.Set.t
  -> needed:int
  -> Market_stub.t list * int
