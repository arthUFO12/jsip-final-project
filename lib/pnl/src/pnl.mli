open! Core
open Types

type t

(** [create slugs] makes an empty book that can track positions in exactly
    [slugs]: every slug gets a price entry (all prices start at
    {!Price.zero}, no expiration). {!update_position} and
    {!apply_trade_report} raise on slugs not passed here, so call
    {!apply_trade_report} to set real prices before trading. *)
val create : Slug.t list -> Price.t -> t

(** [mem t slug] is true when [slug] was passed to {!create}, i.e. the
    raising functions below are safe to call on it. *)
val mem : t -> Slug.t -> bool

val unrealized_pnl : t -> Price.t
val realized_pnl : t -> Price.t
val cash : t -> Price.t

(** Signed contracts held per market (positive long Yes, negative long No),
    with an entry for every slug passed to {!create} — zero when flat. A
    fresh table each call; mutating it does not affect [t]. *)
val inventories : t -> Size.t Slug.Table.t

(** Average price paid per contract of each open position
    ([cost_basis / |inventory|]), with an entry for every slug passed to
    {!create} — zero when flat. A fresh table each call, like {!inventories}. *)
val average_cost_bases : t -> Price.t Slug.Table.t

val update_position
  :  t
  -> slug:Slug.t
  -> side:Side.t
  -> contract_type:Contract_type.t
  -> size:Size.t
  -> unit

module Trade_effect : sig
  type t =
    { cash_after : Price.t
    ; enlarges_position : bool
    (** True when |inventory| grows; a flip past flat counts, because its
        opening leg buys more contracts than the closing leg frees up. *)
    }
end

(** What {!update_position} with the same arguments would do to cash and the
    position, without applying it. Shares the trade math with
    {!update_position}, so callers can gate trades (e.g. refuse to let cash
    go negative) and then apply exactly what they previewed. Raises like
    {!update_position} on slugs not passed to {!create}. *)
val trade_effect
  :  t
  -> slug:Slug.t
  -> side:Side.t
  -> contract_type:Contract_type.t
  -> size:Size.t
  -> Trade_effect.t

val apply_trade_report
  :  slug:Slug.t
  -> ?yes_bbo:Bbo.t
  -> ?no_bbo:Bbo.t
  -> t
  -> unit

(** Record that [slug]'s market settles at [date] with [winner] taking the
    $1-per-contract payout. Raises if [slug] was not passed to {!create}. *)
val set_expiration
  :  t
  -> slug:Slug.t
  -> date:Time_ns.t
  -> winner:Contract_type.t
  -> unit

(** Settle every market whose expiration date is strictly before [now]:
    winning inventory converts to $1 per contract of cash, losing inventory
    is written off against realized pnl, and the position goes flat either
    way. Markets with no recorded expiration are left alone. Safe to call
    repeatedly. *)
val settle_expired_bets : t -> now:Time_ns.t -> unit
