open! Core
open Types
module T = Caqti_type

let int64_to_time_ns num =
  num
  |> Int64.to_int_exn
  |> Time_ns.Span.of_int_sec
  |> Time_ns.of_span_since_epoch
;;

let time_ns_to_int64 time =
  time
  |> Time_ns.to_span_since_epoch
  |> Time_ns.Span.to_int_sec
  |> Int64.of_int
;;

let market_id_type =
  let representation = T.string in
  let encode (market_id : Market_id.t) =
    Ok (Market_id.to_string market_id)
  in
  let decode (market_id_str : string) =
    Ok (Market_id.of_string market_id_str)
  in
  T.custom ~encode ~decode representation
;;


let market_stub_type =
  let representation = T.(t5 T.string T.string T.string T.string T.int64) in
  let encode ({ venue; market_id; slug; title; close_time } : Market_stub.t) =
    Ok
      ( Venue.to_string venue
      , Market_id.to_string market_id
      , Slug.to_string slug
      , title
      , Option.value_exn close_time |> time_ns_to_int64 )
  in
  let decode (venue_str, market_id_str, slug_str, title, close_time_int) =
    Ok
      ({ venue = Venue.of_string venue_str
       ; market_id = Market_id.of_string market_id_str
       ; slug = Slug.of_string slug_str
       ; title
       ; close_time = Some (int64_to_time_ns close_time_int)
       }
       : Market_stub.t)
  in
  T.custom ~encode ~decode representation
;;
