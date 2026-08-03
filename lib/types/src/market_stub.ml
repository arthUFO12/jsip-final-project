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
  ; category : Category.t
  ; volume : Volume.t option
  (** Lifetime traded volume; [None] when the venue did not report one. *)
  }
[@@deriving sexp_of]

let to_string t =
  [%string
    "venue: %{t.venue#Venue}, market_id: %{t.market_id#Market_id}, slug: \
     %{t.slug#Slug}, title: %{t.title}, category: %{t.category#Category}, \
     created_time: %{t.created_time#Time_ns}, close_time: \
     %{t.close_time#Time_ns}"]
;;
