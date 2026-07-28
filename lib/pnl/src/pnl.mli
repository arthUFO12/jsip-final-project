open! Core
open Types

type t

val unrealized_pnl : t -> Price.t
val realized_pnl : t -> Price.t
val cash : t -> Price.t

val update_position
  :  t
  -> slug:Slug.t
  -> contract_type:Contract_type.t
  -> size:Size.t
  -> unit

val apply_trade_report
  :  t
  -> slug:Slug.t
  -> yes_bid_price:Price.t
  -> no_bid_price:Price.t
  -> yes_ask_price:Price.t
  -> no_ask_price:Price.t
  -> expiry:Time_ns.t
  -> unit
