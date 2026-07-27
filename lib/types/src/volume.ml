open! Core

type t =
  | Contracts of Size.t
  | Notional of Price.t
[@@deriving sexp_of, compare, equal]
