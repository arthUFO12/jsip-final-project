open! Core

type t =
  { venue : Venue.t
  ; body : string
  ; received : Time_ns.t
  }
[@@deriving sexp_of, fields]

val create : venue:Venue.t -> body:string -> t
