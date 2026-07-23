open! Core

type t [@@deriving sexp, bin_io, compare, equal, hash]

val evaluate_signal : t -> 'a -> bool
