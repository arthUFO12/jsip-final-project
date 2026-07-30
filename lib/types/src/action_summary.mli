(** Snapshot of a bot's book after a batch of actions has been applied.
    Handed to [Bot.on_response] alongside the per-action
    {!Action_response.t}s so a bot can observe the financial effect of its
    trades without owning the pnl bookkeeping itself. *)

open! Core

type t =
  { cash : Price.t (** Cash on hand, net of premiums paid and received. *)
  ; realized_pnl : Price.t (** Profit locked in by closed positions. *)
  ; unrealized_pnl : Price.t
  (** Mark-to-market value of open positions minus their cost basis. *)
  ; inventory : Size.t Slug.Table.t
  (** Signed contracts held per market: positive is long Yes, negative is
      long No, zero is flat. *)
  }
[@@deriving sexp_of]
