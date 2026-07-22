open! Core
open Types
module T = Caqti_type

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
  let representation = T.(t4 T.string T.string T.string T.int64) in
  let encode ({ venue; market_id; title; close_time } : Market_stub.t) =
    Ok
      ( Venue.to_string venue
      , Market_id.to_string market_id
      , title
      , Option.value_exn close_time
        |> Time_ns.to_span_since_epoch
        |> Time_ns.Span.to_int_sec
        |> Int64.of_int )
  in
  let decode (venue_str, market_id_str, title, close_time_int) =
    Ok
      ({ venue = Venue.of_string venue_str
       ; market_id = Market_id.of_string market_id_str
       ; title
       ; close_time =
           Some
             (Int64.to_int close_time_int
              |> Option.value_exn
              |> Time_ns.Span.of_int_sec
              |> Time_ns.of_span_since_epoch)
       }
       : Market_stub.t)
  in
  T.custom ~encode ~decode representation
;;
