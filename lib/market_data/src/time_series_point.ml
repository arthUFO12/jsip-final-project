open! Core
open Types

type t =
  { time : Time_ns.t
  ; yes_price : Price.t
  ; no_price : Price.t
  }
[@@deriving sexp_of]
