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

(* [Volume.t] is stored as its sexp text: exact round-trip without a
   numeric-column encoding for the variant. Rankings sort in OCaml via
   {!Volume.to_float}, never in SQL. *)
let volume_to_string volume = Sexp.to_string (Volume.sexp_of_t volume)
let volume_of_string text = Volume.t_of_sexp (Sexp.of_string text)

(* The nested pairing exists only because Caqti tuples stop at [t8]; the row
   is flat — columns follow the CREATE TABLE order. *)
let market_stub_type =
  (* Caqti tops out at [t8]; the two timestamps ride as a nested pair, which
     Caqti flattens to two ordinary columns. *)
  let representation =
    T.(
      t2
        (t8
           T.string
           T.string
           T.string
           (T.option T.string)
           (T.option T.string)
           T.string
           T.int64
           T.int64)
        (t2 T.string (T.option T.string)))
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
     ; volume
     } :
      Market_stub.t)
    =
    Ok
      ( ( Venue.to_string venue
        , Market_id.to_string market_id
        , Slug.to_string slug
        , Option.map series_ticker ~f:Slug.to_string
        , clob_token_id
        , title
        , time_ns_to_int64 created_time
        , time_ns_to_int64 close_time )
      , (Category.to_string category, Option.map volume ~f:volume_to_string)
      )
  in
  let decode
    ( ( venue_str
      , market_id_str
      , slug_str
      , series_ticker_str
      , clob_token_id
      , title
      , created_time_int
      , close_time_int )
    , (category_str, volume_str) )
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
       ; volume = Option.map volume_str ~f:volume_of_string
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

let wallet_entry_type =
  let representation =
    T.(t2 (t4 T.string T.string T.float T.int) (t3 T.float T.bool T.float))
  in
  let encode
    ({ pair_key; summary; edge; size; dollars; acted; acted_dollars } :
      Wallet_entry.t)
    =
    Ok ((pair_key, summary, edge, size), (dollars, acted, acted_dollars))
  in
  let decode
    ((pair_key, summary, edge, size), (dollars, acted, acted_dollars))
    =
    Ok
      ({ pair_key; summary; edge; size; dollars; acted; acted_dollars }
       : Wallet_entry.t)
  in
  T.custom ~encode ~decode representation
;;

let arb_observation_type =
  let representation =
    T.(t2 (t2 T.int64 T.string) (t3 T.float T.int T.float))
  in
  let encode ({ at; pair_key; edge; size; dollars } : Arb_observation.t) =
    Ok ((time_ns_to_int64 at, pair_key), (edge, size, dollars))
  in
  let decode ((at_int, pair_key), (edge, size, dollars)) =
    Ok
      ({ Arb_observation.at = int64_to_time_ns at_int
       ; pair_key
       ; edge
       ; size
       ; dollars
       }
       : Arb_observation.t)
  in
  T.custom ~encode ~decode representation
;;

let trade_log_entry_type =
  let representation =
    T.(
      t2
        (t4 T.int64 T.string T.string T.string)
        (t4 (T.option T.string) T.string T.string T.float))
  in
  let encode
    ({ at
     ; venue
     ; market_id
     ; action
     ; client_order_id
     ; outcome
     ; detail
     ; dollars
     } :
      Trade_log_entry.t)
    =
    Ok
      ( (time_ns_to_int64 at, venue, Market_id.to_string market_id, action)
      , (client_order_id, outcome, detail, dollars) )
  in
  let decode
    ( (at_int, venue, market_id_str, action)
    , (client_order_id, outcome, detail, dollars) )
    =
    Ok
      ({ at = int64_to_time_ns at_int
       ; venue
       ; market_id = Market_id.of_string market_id_str
       ; action
       ; client_order_id
       ; outcome
       ; detail
       ; dollars
       }
       : Trade_log_entry.t)
  in
  T.custom ~encode ~decode representation
;;
