open! Core

type t =
  { venue : Venue.t
  ; market_id : Market_id.t
  ; title : string
  ; close_time : Time_ns.t option
  }

let to_string t =
  let closing_time =
    match t.close_time with
    | None -> ""
    | Some time -> Time_ns.to_string time
  in
  [%string
    "venue: %{t.venue#Venue}, market_id: %{t.market_id#Market_id}, title: \
     %{t.title}, close_time: %{closing_time}"]
;;
