open! Core


type t = {
  venue : Venue.t
; market_id : Market_id.t 
; title : string 
; close_time : Time_ns.t option
}

val to_string : t -> string