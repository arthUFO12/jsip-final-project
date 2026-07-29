open! Core

type t =
  { cash : Price.t
  ; realized_pnl : Price.t
  ; unrealized_pnl : Price.t
  }
[@@deriving sexp_of]
