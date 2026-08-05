open! Core

type t =
  { venue : Venue.t
  ; market_id : Market_id.t
  ; slug : Slug.t
  ; series_ticker : Slug.t option
  ; clob_token_id : string option
  ; title : string
  ; category : Category.t
  ; created_time : Time_ns.t
  ; close_time : Time_ns.t
  ; volume : Volume.t option
  (** Lifetime traded volume; [None] when the venue did not report one. *)
  }
[@@deriving sexp]

val to_string : t -> string
