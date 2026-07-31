open! Core

type t =
  { cash : Price.t
  ; realized_pnl : Price.t
  ; unrealized_pnl : Price.t
  ; inventory : Size.t Slug.Table.t
  }
[@@deriving sexp_of]
