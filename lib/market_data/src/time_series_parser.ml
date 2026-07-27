open! Core
open! Async
open Types
module Json = Yojson.Safe
module U = Json.Util

(* Both venues report sample times as unix seconds, e.g. 1753488009. *)
let time_of_unix_seconds json =
  json |> U.to_int |> Time_ns.Span.of_int_sec |> Time_ns.of_span_since_epoch
;;

let polymarket_to_time_series_point (obj : Json.t) : Time_series.Point.t =
  let time = obj |> U.member "t" |> time_of_unix_seconds in
  let yes_price =
    (* [to_number] rather than [to_float]: round prices like 0.5 can arrive
       as JSON ints. *)
    obj |> U.member "p" |> U.to_number |> Price.of_float_dollars
  in
  { time; yes_price; no_price = Price.( - ) Price.one yes_price }
;;

let kalshi_to_time_series_point (obj : Json.t) :Time_series.Point.t =
  let time = obj |> U.member "end_period_ts" |> time_of_unix_seconds in
  (* Kalshi's *_dollars fields are decimal strings, e.g. "0.1400". *)
  let close_dollars side =
    obj
    |> U.member side
    |> U.member "close_dollars"
    |> U.to_string
    |> Float.of_string
  in
  let yes_ask = close_dollars "yes_ask" in
  let yes_bid = close_dollars "yes_bid" in
  let yes_price =
    Float.O.((yes_ask + yes_bid) / 2.) |> Price.of_float_dollars
  in
  { time; yes_price; no_price = Price.( - ) Price.one yes_price }
;;

let parse_kalshi_time_series body =
  let json = Json.from_string body in
  json
  |> U.member "candlesticks"
  |> U.to_list
  |> List.map ~f:kalshi_to_time_series_point
;;

let parse_polymarket_time_series body =
  let json = Json.from_string body in
  json
  |> U.member "history"
  |> U.to_list
  |> List.map ~f:polymarket_to_time_series_point
;;
