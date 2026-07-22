open! Core

type t =
  { venue : Venue.t
  ; market_id : string
  ; title : string

  ; slug : string
  ; yes_bid : Price.t option
  ; yes_ask : Price.t option
  ; last_price : Price.t option
  ; active : bool
  ; close_time : Time_ns.t option
  }
[@@deriving sexp_of, fields]
