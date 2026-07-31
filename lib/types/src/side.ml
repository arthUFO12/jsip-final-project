open! Core

type t =
  | Buy
  | Sell
[@@deriving sexp, compare, equal]

let flip = function Buy -> Sell | Sell -> Buy
