open! Core

type t =
  | Buy
  | Sell
[@@deriving sexp_of, compare, equal, hash, bin_io]

val flip : t -> t
