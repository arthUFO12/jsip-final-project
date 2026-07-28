open! Core

type t =
  | Yes
  | No

val flip : t -> t
val sign : t -> Sign.t
val ( = ) : t -> t -> bool
