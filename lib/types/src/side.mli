open! Core

type t =
  | Buy
  | Sell
[@@deriving sexp, compare, equal]

val flip : t -> t
