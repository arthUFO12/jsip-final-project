open! Core

type t =
  | Buy
  | Sell
[@@deriving sexp_of, compare, equal, hash]

let flip = function Buy -> Sell | Sell -> Buy
