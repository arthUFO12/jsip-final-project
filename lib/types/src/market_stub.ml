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

let to_string t =
  let closing_time =
    match t.close_time with
    | None -> ""
    | Some time -> Time_ns.to_string time
  in
  [%string
    "venue: %{t.venue#Venue}, market_id: %{t.market_id#Market_id}, slug: \
     %{t.slug#Slug}, title: %{t.title}, category: %{t.category#Category}, \
     close_time: %{closing_time}"]
;;
