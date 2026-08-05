open! Core

type t =
  { at : Time_ns.t
  ; venue : string
  ; market_id : Market_id.t
  ; action : string
  ; client_order_id : string option
  ; outcome : string
  ; detail : string
  ; dollars : float
  }
[@@deriving sexp_of]
