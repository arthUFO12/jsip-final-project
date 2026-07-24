open! Core

type t =
  { venue : Venue.t
  ; body : string
  ; received : Time_ns.t
  }
[@@deriving sexp_of, fields]

let create ~venue ~body = { venue; body; received = Time_ns.now () }
