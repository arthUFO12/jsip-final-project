(** The snapshot of the world a rule program is evaluated against on one
    tick. The bot builds a fresh [t] each tick from its own state and
    history; the evaluator ({!Expr.Eval}, {!Signal.evaluate}) only reads it.
    Keeping this a record of values and closures means the parser library
    never depends on a particular bot's [Data.t] representation.

    Prices are exposed in float dollars ([Price.to_dollar_float]) because
    rule arithmetic mixes prices, contract counts, and bare constants; the
    results feed only comparisons and action sizes, never pnl bookkeeping. *)

open! Core
open Types

type t =
  { cash : float (** Latest known cash, in dollars. *)
  ; realized : float (** Latest known realized pnl, in dollars. *)
  ; unrealized : float (** Latest known unrealized pnl, in dollars. *)
  ; price_ago :
      slug:Slug.t
      -> contract:Contract_type.t
      -> ago:Time_ns.Span.t
      -> float option
  (** Price of [contract] on [slug], [ago] before the current tick
      ([Time_ns.Span.zero] = the current tick), in dollars. [None] when the
      bot's history does not reach that far back. *)
  ; inventory : slug:Slug.t -> float
  (** Signed contracts currently held in [slug]: positive long Yes, negative
      long No, [0.] when flat (the {!Pnl.inventories} convention). *)
  ; avg_cost : slug:Slug.t -> float
  (** Average price paid per contract of the open position in [slug], in
      dollars; [0.] when flat (the {!Pnl.average_cost_bases} convention). *)
  }
