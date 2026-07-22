open! Core

type t =
  | Kalshi
  | Polymarket
[@@deriving sexp, compare, equal]
