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
  (* Caqti tops out at [t8]; the two timestamps ride as a nested pair, which
     Caqti flattens to two ordinary columns. *)
  let representation =
    T.(
      t8
        T.string
        T.string
        T.string
        (T.option T.string)
        (T.option T.string)
        T.string
        T.string
        (T.t2 T.int64 T.int64))
  in
  let encode
    ({ venue
     ; market_id
     ; slug
     ; series_ticker
     ; clob_token_id
     ; title
     ; category
     ; created_time
     ; close_time
     } :
      Market_stub.t)
    =
    Ok
      ( Venue.to_string venue
      , Market_id.to_string market_id
      , Slug.to_string slug
      , Option.map series_ticker ~f:Slug.to_string
      , clob_token_id
      , title
      , Category.to_string category
      , (time_ns_to_int64 created_time, time_ns_to_int64 close_time) )
  in
  let decode
    ( venue_str
    , market_id_str
    , slug_str
    , series_ticker_str
    , clob_token_id
    , title
    , category_str
    , (created_time_int, close_time_int) )
    =
    Ok
      ({ venue = Venue.of_string venue_str
       ; market_id = Market_id.of_string market_id_str
       ; slug = Slug.of_string slug_str
       ; series_ticker = Option.map series_ticker_str ~f:Slug.of_string
       ; clob_token_id
       ; title
       ; category = Category.of_string category_str
       ; created_time = int64_to_time_ns created_time_int
       ; close_time = int64_to_time_ns close_time_int
       }
       : Market_stub.t)
  in
  T.custom ~encode ~decode representation
;;

let pair_status_type =
  let representation = T.string in
  let encode (status : Pair_proposal.Status.t) =
    Ok (Pair_proposal.Status.to_string status)
  in
  let decode status_str = Ok (Pair_proposal.Status.of_string status_str) in
  T.custom ~encode ~decode representation
;;

let pair_proposal_type =
  let representation =
    T.(t5 T.string T.string T.float (T.option T.string) T.string)
  in
  let encode ({ left; right; score; explanation; status } : Pair_proposal.t) =
    Ok
      ( Market_id.to_string left
      , Market_id.to_string right
      , score
      , explanation
      , Pair_proposal.Status.to_string status )
  in
  let decode (left_str, right_str, score, explanation, status_str) =
    Ok
      ({ left = Market_id.of_string left_str
       ; right = Market_id.of_string right_str
       ; score
       ; explanation
       ; status = Pair_proposal.Status.of_string status_str
       }
       : Pair_proposal.t)
  in
  T.custom ~encode ~decode representation
;;
