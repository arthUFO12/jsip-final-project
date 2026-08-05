open! Core
open! Async

(** The one switch between pretending and trading. A strategy — the arbitrage
    bot, or any {!Bot_runtime} bot — takes an [Executor.t] and calls
    {!place_order}; whether that fills a simulator or moves real money was
    decided exactly once, where the [t] was built from user config. Nothing
    downstream branches on the mode again, so simulated and live runs
    exercise the same strategy code path.

    [paper] wraps a {!Simulator.t}. [live] wraps Kalshi credentials; a live
    Polymarket order is rejected with an error (its CLOB needs an EIP-712
    wallet signature we do not support), so a live run can only ever spend
    money on Kalshi. *)
type t

val paper : Simulator.t -> t
val live : Kalshi_live.Credentials.t -> t

(** For callers that must refuse a dangerous combination up front — e.g. the
    arbitrage bot rejects live execution under reckless detection. *)
val is_live : t -> bool

(** [client_order_id] is the live path's idempotency handle (see
    {!Kalshi_live.place_order}); the simulator ignores it. *)
val place_order
  :  ?client_order_id:string
  -> t
  -> Order.t
  -> Fill.t Deferred.Or_error.t
