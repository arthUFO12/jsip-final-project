open! Core

type t =
  | Yes
  | No
[@@deriving sexp, compare, equal]

val flip : t -> t
val sign : t -> Sign.t
