open! Core

type t =
  | Buy
  | Sell

let flip = function Buy -> Sell | Sell -> Buy
