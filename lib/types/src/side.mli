open! Core

type t =
  | Buy
  | Sell
[@@deriving sexp_of, compare, equal, hash]

val flip : t -> t
