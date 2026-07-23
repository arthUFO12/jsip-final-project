open! Core

type t [@@deriving sexp, bin_io, compare, equal, hash, string]

include Comparable.S with type t := t

val zero : t
val neg : t -> t
val abs : t -> t
val sign : t -> Sign.t
val to_int : t -> int
val of_int : int -> t
val min : t -> t -> t
val multiply_by_price : t -> Price.t -> Price.t
val divide_price : Price.t -> t -> Price.t
val ( - ) : t -> t -> t
val ( + ) : t -> t -> t
val ( * ) : t -> int -> t
