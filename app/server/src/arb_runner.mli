open! Core
open! Async

(** The server half of the arbitrage pipeline: one function per {!Protocol}
    RPC, each delegating to the same {!Arbitrage} modules the CLI verbs use,
    so the browser and the terminal always describe the same store. All wire
    conversion (status mirroring, prices as float dollars) happens here.

    Requires {!Database.Database_exec.init_database} and both table creates
    to have run — [server.ml]'s startup does this. *)

(** Backs {!Protocol.get_pairs}. *)
val list_pairs
  :  Protocol.Pair_status.t
  -> Protocol.Pair_card.t list Deferred.Or_error.t

(** Backs {!Protocol.decide_pair}. *)
val decide
  :  Protocol.Decide_request.t
  -> Protocol.Pair_card.t Deferred.Or_error.t

(** Backs {!Protocol.run_sweep}: a full-listing text-only
    {!Arbitrage.Sweep.sweep_full} — never the LLM. *)
val sweep
  :  Protocol.Sweep_request.t
  -> Protocol.Sweep_summary.t Deferred.Or_error.t

(** Backs {!Protocol.scan_edges}: one detection tick over the approved pairs,
    pricing each against both venues' live books without placing orders. *)
val scan : unit -> Protocol.Scan_report.t Deferred.Or_error.t
