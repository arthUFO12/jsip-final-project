open! Core

type t =
  { venue : Venue.t
  ; market_id : Market_id.t
  ; slug : Slug.t
  ; series_ticker : Slug.t option
  ; clob_token_id : string option
  ; title : string
  ; category : Category.t
  ; close_time : Time_ns.t option
  }
[@@deriving sexp_of]

val to_string : t -> string
