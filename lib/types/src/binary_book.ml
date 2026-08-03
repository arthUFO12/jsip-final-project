open! Core

type t =
  { yes_ask : Price.t
  ; yes_ask_size : Size.t
  ; no_ask : Price.t
  ; no_ask_size : Size.t
  }
[@@deriving sexp_of]
