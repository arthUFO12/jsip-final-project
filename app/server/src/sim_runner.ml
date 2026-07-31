open! Core
open! Async
open Types
open Market_data
open Database

let initial_cash = Price.of_int_cents 10_000 (* $100, as in the CLI *)
let max_markets = 4
let max_lookback_days = 30

let interval_of_wire : Protocol.Interval.t -> Time_series.Interval.t
  = function
  | Minute -> Minute
  | Hour -> Hour
  | Day -> Day
;;

let validate
  ({ slugs; program; interval = _; lookback_days; warmup_hours } :
    Protocol.Sim_request.t)
  =
  let market_count = List.length slugs in
  if market_count < 1 || market_count > max_markets
  then
    Or_error.error_s
      [%message "choose between 1 and 4 markets" (market_count : int)]
  else if String.is_empty (String.strip program)
  then Or_error.error_string "the bot program is empty"
  else if lookback_days < 1 || lookback_days > max_lookback_days
  then
    Or_error.error_s
      [%message
        "lookback must be between 1 and 30 days" (lookback_days : int)]
  else if warmup_hours < 0
  then
    Or_error.error_s
      [%message "warmup cannot be negative" (warmup_hours : int)]
  else if Float.O.(
            Float.of_int warmup_hours >= Float.of_int lookback_days *. 24.)
  then
    Or_error.error_s
      [%message
        "warmup must be shorter than the lookback window"
          (warmup_hours : int)
          (lookback_days : int)]
  else Ok ()
;;

(* The seed is the market universe: a slug the client sends must resolve
   there, and only stubs with a series ticker can serve price history. *)
let resolve_stubs slugs =
  Deferred.Or_error.List.map slugs ~how:`Sequential ~f:(fun slug ->
    match%bind.Deferred.Or_error
      Database_exec.find_market_stub_by_slug slug
    with
    | None ->
      Deferred.return
        (Or_error.error_s [%message "unknown market" (slug : Slug.t)])
    | Some stub ->
      (match stub.series_ticker with
       | None ->
         Deferred.return
           (Or_error.error_s
              [%message "market cannot serve price history" (slug : Slug.t)])
       | Some (_ : Slug.t) -> Deferred.Or_error.return stub))
;;

let fill_of_response (response : Action_response.t) : Protocol.Fill.t =
  let of_action (action : Action.t) time_stamp rejected : Protocol.Fill.t =
    { time_s = Protocol.epoch_seconds time_stamp
    ; id = action.id
    ; side = action.side
    ; contract = action.contract_type
    ; size = Size.to_int action.size
    ; slug = action.slug
    ; rejected
    }
  in
  match response with
  | Action_accepted { action; time_stamp } ->
    of_action action time_stamp None
  | Action_rejected { action; time_stamp; reason } ->
    of_action action time_stamp (Some reason)
;;

let tick_point
  ({ time; cash; realized_pnl; unrealized_pnl; yes_prices } :
    Simulation.Harness.Recording.Tick.t)
  : Protocol.Tick_point.t
  =
  { time_s = Protocol.epoch_seconds time
  ; cash = Price.to_dollar_float cash
  ; realized = Price.to_dollar_float realized_pnl
  ; unrealized = Price.to_dollar_float unrealized_pnl
  ; yes_prices =
      Hashtbl.to_alist yes_prices
      |> List.map ~f:(fun (slug, price) -> slug, Price.to_dollar_float price)
      |> List.sort ~compare:(fun (left, _) (right, _) ->
        Slug.compare left right)
  }
;;

let run (request : Protocol.Sim_request.t) =
  let open Deferred.Or_error.Let_syntax in
  let%bind () = Deferred.return (validate request) in
  let%bind stubs = resolve_stubs request.slugs in
  let%bind rules =
    Deferred.return
      (Parser.Parse.program request.program ~slugs:request.slugs)
  in
  let%bind () = Fetch_time_series.update_kalshi_historical_cutoff () in
  let interval = interval_of_wire request.interval in
  let probe_finish = Time_ns.now () in
  let probe_start =
    Time_ns.sub
      probe_finish
      (Time_ns.Span.of_day (Float.of_int request.lookback_days))
  in
  let%bind series_by_stub =
    Market_data_gateway.fetch_many_ticker_series
      stubs
      ~start:probe_start
      ~finish:probe_finish
      ~interval
  in
  let%bind start, finish =
    Deferred.return (Time_series.shared_window series_by_stub)
  in
  let%bind config =
    Deferred.return
      (Bots.Configurable.Config.create
         ~id:0
         ~start
         ~finish
         ~interval
         ~sim_start_offset:
           (Time_ns.Span.of_hr (Float.of_int request.warmup_hours))
         ~initial_cash
         ~slugs:request.slugs
         ~rules)
  in
  let%bind recording =
    Simulation.Harness.run_recorded
      stubs
      (Simulation.Harness.P ((module Bots.Configurable.Bot), config))
  in
  return
    ({ ticks = List.map recording.ticks ~f:tick_point
     ; fills = List.map recording.responses ~f:fill_of_response
     ; sim_start_s = Protocol.epoch_seconds recording.sim_start
     }
     : Protocol.Sim_result.t)
;;
