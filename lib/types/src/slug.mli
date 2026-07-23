open! Core

type t [@@deriving sexp, bin_io, compare, equal, hash, string]

include Hashable.S with type t := t

val of_string : string -> t
val to_string : t -> string
