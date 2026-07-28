open! Core
open Types

type t

val unrealized_pnl : t -> Price.t
val realized_pnl : t -> Price.t
val cash : t -> Price.t

val update_position
  :  t
  -> slug:Slug.t
  -> side:Side.t
  -> contract_type:Contract_type.t
  -> size:Size.t
  -> unit

val apply_trade_report
  :  slug:Slug.t
  -> ?yes_bbo:Bbo.t
  -> ?no_bbo:Bbo.t
  -> t
  -> unit
