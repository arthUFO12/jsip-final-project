open! Core
open! Async
open Types
open Market_data
open Simulation

module Config = struct
  type t =
    { id : int
    ; start : Time_ns.t
    ; finish : Time_ns.t
    ; interval : Time_series.Interval.t
    ; sim_start_offset : Time_ns.Span.t
    ; initial_cash : Price.t
    ; trade_probability : float
    ; max_size : int
    ; seed : int
    }
  [@@deriving sexp, fields ~getters]

  let num_markets = 4
  let market_fetch_limit = 50
  let probe_window = Time_ns.Span.of_day 21.
  let default_interval = Time_series.Interval.Hour
  let default_sim_start_offset = Time_ns.Span.of_hr 12.
  let default_trade_probability = 0.2
  let default_max_size = 100
  let default_initial_cash = Price.of_int_cents 10_000 (* $100 *)

  let choose_markets metadata =
    let stubs =
      List.filter metadata ~f:(fun (market : L1_market_metadata.t) ->
        market.active && Option.is_some market.series_ticker)
      |> List.map ~f:L1_market_metadata.to_market_stub
    in
    let chosen = List.take stubs num_markets in
    match List.length chosen = num_markets with
    | true -> Ok chosen
    | false ->
      Or_error.error_s
        [%message
          "not enough tradeable kalshi markets"
            ~wanted:(num_markets : int)
            ~found:(List.length chosen : int)]
  ;;

  (* The window where every chosen market has data: latest first entry to
     earliest last entry. *)
  let shared_window series_by_stub =
    let open Or_error.Let_syntax in
    let%bind bounds =
      List.map series_by_stub ~f:(fun ((stub : Market_stub.t), series) ->
        match List.hd series, List.last series with
        | ( Some (first : Time_series.Point.t)
          , Some (last : Time_series.Point.t) ) ->
          Ok (first.time, last.time)
        | _, _ ->
          Or_error.error_s
            [%message
              "no recent time series data" ~market:(stub.slug : Slug.t)])
      |> Or_error.combine_errors
    in
    let start =
      List.map bounds ~f:fst
      |> List.max_elt ~compare:Time_ns.compare
      |> Option.value_exn ~here:[%here]
    in
    let finish =
      List.map bounds ~f:snd
      |> List.min_elt ~compare:Time_ns.compare
      |> Option.value_exn ~here:[%here]
    in
    match Time_ns.( < ) start finish with
    | true -> Ok (start, finish)
    | false ->
      Or_error.error_s
        [%message
          "chosen markets have no overlapping data"
            (start : Time_ns.t)
            (finish : Time_ns.t)]
  ;;

  let create ~id ~seed =
    let open Deferred.Or_error.Let_syntax in
    let%bind metadata =
      Market_data_gateway.fetch_l1_market_data
        ~venue:Kalshi
        ~closed:false
        ~limit:market_fetch_limit
        ()
    in
    let%bind stubs = Deferred.return (choose_markets metadata) in
    let%bind () = Fetch_time_series.update_kalshi_historical_cutoff () in
    let probe_finish = Time_ns.now () in
    let probe_start = Time_ns.sub probe_finish probe_window in
    let%bind series_by_stub =
      Market_data_gateway.fetch_many_ticker_series
        stubs
        ~start:probe_start
        ~finish:probe_finish
        ~interval:default_interval
    in
    let%bind start, finish =
      Deferred.return (shared_window series_by_stub)
    in
    return
      ( { id
        ; start
        ; finish
        ; interval = default_interval
        ; sim_start_offset = default_sim_start_offset
        ; initial_cash = default_initial_cash
        ; trade_probability = default_trade_probability
        ; max_size = default_max_size
        ; seed
        }
      , stubs )
  ;;
end

module Bot : Bot_interface.Bot with type Config.t = Config.t = struct
  module Config = Config

  module State = struct
    type t =
      { mutable now : Time_ns.t
      ; rng : Random.State.t
      ; generator : Action.Generator.t
      }

    let forward_time t now = t.now <- now
  end

  module Data = struct
    (* The latest known point per market. *)
    type t = Time_series.Point.t Slug.Table.t
  end

  let name = "basic"

  let init_data series (_ : Config.t) : Data.t =
    Hashtbl.filter_map series ~f:List.hd
  ;;

  let update_data (_ : Config.t) (data : Data.t) points : Data.t =
    let data = Hashtbl.copy data in
    Hashtbl.iteri points ~f:(fun ~key ~data:point ->
      Hashtbl.set data ~key ~data:point);
    data
  ;;

  let create_state (config : Config.t) : State.t =
    { now = config.start
    ; rng = Random.State.make [| config.seed; config.id |]
    ; generator = Action.Generator.create ()
    }
  ;;

  let on_start (_ : Config.t) (_ : State.t) = ()

  let decide_trade (config : Config.t) (state : State.t) (slug : Slug.t)
    : Action.t option
    =
    let uniform_rv = Random.State.float state.rng 1. in
    if Float.O.(uniform_rv < config.trade_probability)
    then (
      let uniform_rv1 = Random.State.float state.rng 1. in
      let uniform_rv2 = Random.State.float state.rng 1. in
      let side = if Float.O.(uniform_rv1 < 0.5) then Side.Buy else Sell in
      let contract_type =
        if Float.O.(uniform_rv2 < 0.5) then Contract_type.Yes else No
      in
      Some
        (Action.Generator.new_action
           state.generator
           ~size:
             (Random.State.int_incl state.rng 1 config.max_size
              |> Size.of_int)
           ~side
           ~slug
           ~contract_type
           ()))
    else None
  ;;

  let on_tick (config : Config.t) (state : State.t) (data : Data.t) =
    List.filter_map (Hashtbl.keys data) ~f:(decide_trade config state)
  ;;

  let on_response
    (_ : Config.t)
    (_ : State.t)
    (_responses : Action_response.t list)
    ({ cash = _
     ; realized_pnl = _
     ; unrealized_pnl = _
     ; inventory = _
     ; average_costs = _
     } :
      Action_summary.t)
    =
    ()
  ;;
end
