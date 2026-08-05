open! Core

type t =
  { pair_key : string
  ; summary : string
  ; edge : float
  ; size : int
  ; dollars : float
  ; acted : bool
  ; acted_dollars : float
  }
[@@deriving sexp_of]
