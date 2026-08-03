open! Core

type t =
  | Yes
  | No
[@@deriving sexp_of, compare, equal, hash, bin_io]

val flip : t -> t
val sign : t -> Sign.t
val ( = ) : t -> t -> bool
