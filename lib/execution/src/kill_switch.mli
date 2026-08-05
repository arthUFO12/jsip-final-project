open! Core
open! Async

(** The stop-everything lever, checked immediately before every live order.
    Two independent triggers, either sufficient:

    - the [TRADING_DISABLED] environment variable (any value), set before the
      process started;
    - a [trading.disabled] sentinel file in the working directory, which an
      operator can [touch] while the process is running — and which a remote
      surface may create but, by design, never remove: clearing the switch
      requires access to the machine.

    Checking is cheap (one [stat]), so callers need not cache the answer —
    staleness is exactly what a kill switch must not have. *)

(** The sentinel file name, relative to the working directory. *)
val sentinel_file : string

(** [engaged ()] is [Some reason] when trading must not happen. *)
val engaged : unit -> string option Deferred.t

(** [trip ~reason] creates the sentinel file with [reason] as its contents —
    the remote half of the design above: any surface may stop trading, but
    only someone at the machine can delete the file and resume. Idempotent;
    tripping an already-tripped switch rewrites the reason. *)
val trip : reason:string -> unit Deferred.Or_error.t
