open! Core
open! Async
open Types
open Yojson.Safe.Util

(* Venue APIs take timestamps as unix seconds, e.g. start_ts=1753488009. *)
let unix_seconds_param time =
  Time_ns.to_span_since_epoch time
  |> Time_ns.Span.to_int_sec
  |> Int.to_string
;;

let interval_to_param : Time_series.Interval.t -> string = function
  | Minute -> "1"
  | Hour -> "60"
  | Day -> "1440"
;;

let request_helper (uri : Uri.t) : string Deferred.Or_error.t =
  let%bind state, body = Cohttp_async.Client.get uri in
  let%map body = Cohttp_async.Body.to_string body in
  match Cohttp.Response.status state with
  | #Cohttp.Code.success_status -> Or_error.return body
  | status ->
    Or_error.error_s
      [%message
        "http request failed" (Cohttp.Code.string_of_status status : string)]
;;

let historical_cutoff : Time_ns.t option ref = ref None

let kalshi_cutoff_uri =
  Uri.of_string
    "https://external-api.kalshi.com/trade-api/v2/historical/cutoff"
;;

let create_kalshi_live_uri ~series_ticker ~slug =
  Uri.of_string
    [%string
      "https://external-api.kalshi.com/trade-api/v2/series/%{series_ticker#Slug}/markets/%{slug#Slug}/candlesticks"]
;;

let create_kalshi_historical_uri ~slug =
  Uri.of_string
    [%string
      "https://external-api.kalshi.com/historical/markets/%{slug#Slug}/candlesticks"]
;;

let polymarket_uri =
  Uri.of_string "https://clob.polymarket.com/prices-history"
;;

let parse_kalshi_historical_cutoff body =
  let json = Yojson.Safe.from_string body in
  json
  |> member "market_settled_ts"
  |> to_string_option
  |> Option.map ~f:Time_ns.of_string
;;

let update_kalshi_historical_cutoff () : unit Deferred.Or_error.t =
  let%bind.Deferred.Or_error body = request_helper kalshi_cutoff_uri in
  historical_cutoff := parse_kalshi_historical_cutoff body;
  Deferred.Or_error.return ()
;;

let fetch_historical_kalshi_data
  (market_stub : Market_stub.t)
  ~start
  ~finish
  ~interval
  =
  let uri = create_kalshi_historical_uri ~slug:market_stub.slug in
  let params =
    [ "start_ts", unix_seconds_param start
    ; "end_ts", unix_seconds_param finish
    ; "period_interval", interval_to_param interval
    ; "include_latest_before_start", "true"
    ]
  in
  let url = Uri.add_query_params' uri params in
  request_helper url
;;

let fetch_live_kalshi_data
  (market_stub : Market_stub.t)
  ~start
  ~finish
  ~interval
  =
  let uri =
    create_kalshi_live_uri
      ~series_ticker:(Option.value_exn market_stub.series_ticker)
      ~slug:market_stub.slug
  in
  let params =
    [ "start_ts", unix_seconds_param start
    ; "end_ts", unix_seconds_param finish
    ; "period_interval", interval_to_param interval
    ; "include_latest_before_start", "true"
    ]
  in
  let url = Uri.add_query_params' uri params in
  request_helper url
;;

let fetch_polymarket_data
  (market_stub : Market_stub.t)
  ~(start : Time_ns.t)
  ~(finish : Time_ns.t)
  ~(interval : Time_series.Interval.t)
  =
  let params =
    [ "market", Option.value_exn market_stub.clob_token_id
    ; "startTs", unix_seconds_param start
    ; "endTs", unix_seconds_param finish
    ; "fidelity", interval_to_param interval
    ]
  in
  let url = Uri.add_query_params' polymarket_uri params in
  request_helper url
;;

let fetch_kalshi_data
  (market_stub : Market_stub.t)
  ~(start : Time_ns.t)
  ~(finish : Time_ns.t)
  ~(interval : Time_series.Interval.t)
  =
  let historical_cutoff = Option.value_exn !historical_cutoff in
  let close_time = market_stub.close_time in
  if Time_ns.O.(close_time < historical_cutoff)
  then fetch_historical_kalshi_data market_stub ~start ~finish ~interval
  else fetch_live_kalshi_data market_stub ~start ~finish ~interval
;;
