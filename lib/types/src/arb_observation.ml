open! Core

type t =
  { at : Time_ns.t
  ; pair_key : string
  ; edge : float
  ; size : int
  ; dollars : float
  }
[@@deriving sexp_of]
