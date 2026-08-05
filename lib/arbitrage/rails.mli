open! Core
open! Async
open! Types

(** The rails between deciding and spending, shared by every live path —
    {!Bot.execute}'s loop and the web server's assisted hedge — so the CLI
    and the browser can never drift on what "inside the rails" means: the
    {!Execution.Kill_switch}, the per-order notional cap, and the per-day cap
    charged against the trade log's ledger (failing closed if the ledger is
    unreadable). Refusals return the reason; callers must not send the order
    and should log the refusal.

    Requires {!Database.Database_exec.init_database}. *)

(** [limit x size] as float dollars — the notional the caps measure. *)
val order_dollars : Execution.Order.t -> float

(** Start of the current UTC day — the per-day cap's window. *)
val utc_day_start : unit -> Time_ns.t

(** [Some reason] when the order must not be sent. *)
val live_refusal
  :  execution:Config.Execution.t
  -> Execution.Order.t
  -> string option Deferred.t

(** Append one live-order event to the audit log; a failed write cannot
    strand the flow but is impossible to miss on stderr. *)
val log_live_order
  :  Execution.Order.t
  -> client_order_id:string
  -> outcome:string
  -> dollars:float
  -> unit Deferred.t
