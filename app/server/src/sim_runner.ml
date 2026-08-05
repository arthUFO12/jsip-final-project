open! Core
open! Async
open Types
open Market_data
open Database

let max_markets = 4
let max_lookback_days = 30
let max_starting_cash_cents = 100_000_000 (* $1M *)
let max_basic_bots = 100
let max_basic_trade_size = 1000

(* Bumped once per request so each run faces a fresh dumb-bot group; bot [i]
   draws from [Random.State.make [| seed; i + 1 |]] (id 0 is the configurable
   bot). Only touched on the scheduler with no bind in between, so concurrent
   requests cannot tear it. *)
let basic_bot_seed_counter = ref 0

let interval_of_wire : Protocol.Interval.t -> Time_series.Interval.t
  = function
  | Minute -> Minute
  | Hour -> Hour
  | Day -> Day
;;

let validate
  ({ slugs
   ; program
   ; variables = _
   ; interval = _
   ; lookback_days
   ; warmup_hours
   ; starting_cash_cents
   ; allow_negative_cash = _
   ; basic_bots
   } :
    Protocol.Sim_request.t)
  =
  let market_count = List.length slugs in
  if market_count < 1 || market_count > max_markets
  then
    Or_error.error_s
      [%message "choose between 1 and 4 markets" (market_count : int)]
  else if String.is_empty (String.strip program)
  then Or_error.error_string "the bot program is empty"
  else if starting_cash_cents < 1
          || starting_cash_cents > max_starting_cash_cents
  then
    Or_error.error_s
      [%message
        "starting cash must be between $0.01 and $1,000,000"
          (starting_cash_cents : int)]
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
  else (
    match basic_bots with
    | None -> Ok ()
    | Some ({ count; trade_probability; max_size } : Protocol.Basic_bots.t)
      ->
      if count < 1 || count > max_basic_bots
      then
        Or_error.error_s
          [%message
            "basic bot count must be between 1 and 100" (count : int)]
      else if Float.O.(trade_probability <= 0. || trade_probability > 1.)
      then
        Or_error.error_s
          [%message
            "basic bot trade probability must be greater than 0 and at most \
             1"
              (trade_probability : float)]
      else if max_size < 1 || max_size > max_basic_trade_size
      then
        Or_error.error_s
          [%message
            "basic bot max trade size must be between 1 and 1000"
              (max_size : int)]
      else Ok ())
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
    ; reason = action.reason
    }
  in
  match response with
  | Action_accepted { action; time_stamp } ->
    of_action action time_stamp None
  | Action_rejected { action; time_stamp; reason } ->
    of_action action time_stamp (Some reason)
;;

let sort_by_slug alist =
  List.sort alist ~compare:(fun (left, _) (right, _) ->
    Slug.compare left right)
;;

let tick_point
  ({ time; cash; realized_pnl; unrealized_pnl; yes_prices; inventories } :
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
      |> sort_by_slug
  ; inventory =
      Hashtbl.to_alist inventories
      |> List.map ~f:(fun (slug, size) -> slug, Size.to_int size)
      |> sort_by_slug
  }
;;

(* What selling every held contract at this tick's marked Yes prices would
   fetch. Must mirror the client's [portfolio_value] so the baseline and
   configurable lines are comparable on the value chart. *)
let tick_portfolio_value (tick : Simulation.Harness.Recording.Tick.t) : float
  =
  Hashtbl.fold
    tick.inventories
    ~init:Price.zero
    ~f:(fun ~key:slug ~data:inventory acc ->
      let yes_price = Hashtbl.find_exn tick.yes_prices slug in
      if Size.(inventory >= Size.zero)
      then Price.(acc + Size.multiply_by_price inventory yes_price)
      else
        Price.(
          acc
          + Size.multiply_by_price
              (Size.abs inventory)
              Price.(Price.one - yes_price)))
  |> Price.to_dollar_float
;;

(* Ticks are index-aligned across recordings (same grid: identical
   start/finish/interval and a config-independent data start), so averaging
   is a transpose plus a per-column mean. Misalignment is an internal
   invariant violation, reported rather than raised. *)
let baseline_points (recordings : Simulation.Harness.Recording.t list)
  : Protocol.Baseline_point.t list Or_error.t
  =
  match List.transpose (List.map recordings ~f:(fun r -> r.ticks)) with
  | None ->
    Or_error.error_s
      [%message
        "bug: basic bot tick histories are misaligned"
          ~lengths:
            (List.map recordings ~f:(fun r -> List.length r.ticks)
             : int list)]
  | Some columns ->
    Ok
      (List.map columns ~f:(fun column ->
         let count = Float.of_int (List.length column) in
         let mean f = List.sum (module Float) column ~f /. count in
         let money f (tick : Simulation.Harness.Recording.Tick.t) =
           Price.to_dollar_float (f tick)
         in
         { Protocol.Baseline_point.time_s =
             Protocol.epoch_seconds (List.hd_exn column).time
         ; cash = mean (money (fun tick -> tick.cash))
         ; realized = mean (money (fun tick -> tick.realized_pnl))
         ; unrealized = mean (money (fun tick -> tick.unrealized_pnl))
         ; portfolio_value = mean tick_portfolio_value
         }))
;;

(* Percentile rank (0..100) of the configurable bot's final total pnl among
   the dumb bots' final total pnls. *)
let pnl_percentile
  (recording : Simulation.Harness.Recording.t)
  (basic_recordings : Simulation.Harness.Recording.t list)
  : float
  =
  let tot_pnl (reco : Simulation.Harness.Recording.t) =
    Price.to_dollar_float reco.summary.unrealized_pnl
    +. Price.to_dollar_float reco.summary.realized_pnl
  in
  let configurable_pnl = tot_pnl recording in
  let leq =
    List.fold basic_recordings ~init:0. ~f:(fun acc basic_recording ->
      let basic_pnl = tot_pnl basic_recording in
      if Float.O.(basic_pnl < configurable_pnl)
      then acc +. 1.
      else if Float.O.(basic_pnl = configurable_pnl)
      then acc +. 0.5
      else acc)
  in
  100. *. leq /. (List.length basic_recordings |> Int.to_float)
;;

(* One domain per running simulation, capped below the core count so the
   Async scheduler's domain keeps a core. [In_thread.run] keeps the blocking
   [Domain.join] off the scheduler thread; the spawned closure is pure
   compute and must stay Async-free. *)
let sim_throttle =
  lazy
    (Throttle.create
       ~continue_on_error:true
       ~max_concurrent_jobs:(max 1 (Domain.recommended_domain_count () - 1)))
;;

let run_recorded_in_domain
  stubs
  series_by_stub
  packed_bot
  ~allow_negative_cash
  : Simulation.Harness.Recording.t Deferred.Or_error.t
  =
  Throttle.enqueue (force sim_throttle) (fun () ->
    let domain =
      Domain.spawn (fun () ->
        Simulation.Harness.run_recorded
          stubs
          series_by_stub
          packed_bot
          ~allow_negative_cash)
    in
    In_thread.run (fun () -> Domain.join domain))
;;

let run (request : Protocol.Sim_request.t) =
  let open Deferred.Or_error.Let_syntax in
  let%bind () = Deferred.return (validate request) in
  let%bind stubs = resolve_stubs request.slugs in
  (* Definitions precede the rules in one parse, so rule parse errors report
     line numbers offset by the number of variables. *)
  let source =
    String.concat ~sep:"\n" (request.variables @ [ request.program ])
  in
  let%bind rules =
    Deferred.return (Parser.Parse.program source ~slugs:request.slugs)
  in
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
  let sim_start_offset =
    Time_ns.Span.of_hr (Float.of_int request.warmup_hours)
  in
  let initial_cash = Price.of_int_cents request.starting_cash_cents in
  let%bind config =
    Deferred.return
      (Bots.Configurable.Config.create
         ~id:0
         ~start
         ~finish
         ~interval
         ~sim_start_offset
         ~initial_cash
         ~slugs:request.slugs
         ~rules)
  in
  let basic_configs =
    match request.basic_bots with
    | None -> []
    | Some ({ count; trade_probability; max_size } : Protocol.Basic_bots.t)
      ->
      incr basic_bot_seed_counter;
      let seed = !basic_bot_seed_counter in
      List.init count ~f:(fun index : Bots.Basic.Config.t ->
        { id = index + 1
        ; start
        ; finish
        ; interval
        ; sim_start_offset
        ; initial_cash
        ; trade_probability
        ; max_size
        ; seed
        })
  in
  (* Enqueue every job before awaiting any: the throttle runs them in
     parallel domains, [max_concurrent_jobs] at a time. *)
  let configurable_job =
    run_recorded_in_domain
      stubs
      series_by_stub
      (Simulation.Harness.P ((module Bots.Configurable.Bot), config))
      ~allow_negative_cash:request.allow_negative_cash
  in
  let basic_jobs =
    List.map basic_configs ~f:(fun basic_config ->
      run_recorded_in_domain
        stubs
        series_by_stub
        (Simulation.Harness.P ((module Bots.Basic.Bot), basic_config))
        ~allow_negative_cash:request.allow_negative_cash)
  in
  let%bind recording = configurable_job in
  let%bind basic_recordings = Deferred.Or_error.all basic_jobs in
  let%bind baseline_ticks =
    match basic_recordings with
    | [] -> return []
    | _ :: _ -> Deferred.return (baseline_points basic_recordings)
  in
  return
    ({ ticks = List.map recording.ticks ~f:tick_point
     ; fills = List.map recording.responses ~f:fill_of_response
     ; sim_start_s = Protocol.epoch_seconds recording.sim_start
     ; baseline_ticks
     ; pnl_percentile =
         (match basic_recordings with
          | [] -> None
          | _ :: _ -> Some (pnl_percentile recording basic_recordings))
     }
     : Protocol.Sim_result.t)
;;
