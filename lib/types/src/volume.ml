open! Core

type t =
  | Contracts of Size.t
  | Notional of Price.t
[@@deriving sexp, bin_io, compare, equal, hash]

let to_float = function
  | Contracts size -> Float.of_int (Size.to_int size)
  | Notional price -> Price.to_dollar_float price
;;
