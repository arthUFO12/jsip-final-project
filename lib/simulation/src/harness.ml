open! Core
open! Async
open Types
open Market_data

type 'a bot = (module Bot_interface.Bot with type Config.t = 'a)
type packed_bot = P : 'a bot * 'a -> packed_bot

(* The simulation window must sit inside every market's lifetime: a market
   has no prices before [created_time] and settles at [close_time]. *)
let validate_simulation_window market_stubs ~start ~finish =
  List.map market_stubs ~f:(fun (stub : Market_stub.t) ->
    if Time_ns.( < ) start stub.created_time
    then
      Or_error.error_s
        [%message
          "simulation starts before market was created"
            ~market:(stub.slug : Slug.t)
            (start : Time_ns.t)
            ~created_time:(stub.created_time : Time_ns.t)]
    else if Time_ns.( > ) finish stub.close_time
    then
      Or_error.error_s
        [%message
          "simulation ends after market closes"
            ~market:(stub.slug : Slug.t)
            (finish : Time_ns.t)
            ~close_time:(stub.close_time : Time_ns.t)]
    else Ok ())
  |> Or_error.combine_errors_unit
;;

(* Venues do not reliably return history all the way back to the requested
   start (Polymarket in particular), so the usable window begins at the
   latest first entry across the fetched series. Errors if any venue returned
   no data at all — silently dropping that market would leave the bot trading
   a phantom. *)
let data_start_of_series series_by_stub ~requested_start =
  List.map series_by_stub ~f:(fun ((stub : Market_stub.t), series) ->
    match (series : Time_series.t) with
    | [] ->
      Or_error.error_s
        [%message
          "venue returned no time series data" ~market:(stub.slug : Slug.t)]
    | first :: _ -> Ok first.time)
  |> Or_error.combine_errors
  |> Or_error.map ~f:(fun first_times ->
    List.max_elt first_times ~compare:Time_ns.compare
    |> Option.value ~default:requested_start)
;;

let load_aligned_series market_stubs ~start ~finish ~interval =
  let%bind.Deferred.Or_error series_by_stub =
    Market_data_gateway.fetch_many_ticker_series
      market_stubs
      ~start
      ~finish
      ~interval
  in
  Deferred.return
    (let%bind.Or_error data_start =
       data_start_of_series series_by_stub ~requested_start:start
     in
     let%bind.Or_error series_by_slug =
       List.map series_by_stub ~f:(fun ((stub : Market_stub.t), series) ->
         Time_series.interpolate series ~start:data_start ~finish ~interval
         |> Or_error.tag ~tag:[%string "interpolating %{stub.slug#Slug}"]
         |> Or_error.map ~f:(fun series -> stub.slug, series))
       |> Or_error.combine_errors
     in
     Ok (data_start, series_by_slug))
;;

(* Interpolation puts every series on the same time grid, so position [i] of
   each list is the same instant. Truncate to the shortest series in case a
   venue's data ended early. *)
let ticks_of_series series_by_slug =
  match
    List.map series_by_slug ~f:(fun (slug, series) ->
      slug, Array.of_list series)
  with
  | [] -> []
  | (_, first_series) :: _ as series_arrays ->
    let num_ticks =
      List.fold
        series_arrays
        ~init:(Array.length first_series)
        ~f:(fun shortest (_, series) -> min shortest (Array.length series))
    in
    List.init num_ticks ~f:(fun i ->
      let points =
        List.map series_arrays ~f:(fun (slug, series) -> slug, series.(i))
        |> Slug.Table.of_alist_exn
      in
      first_series.(i).Time_series.Point.time, points)
;;

let update_pnl_prices pnl points =
  Hashtbl.iteri
    points
    ~f:
      (fun
        ~key:slug
        ~data:({ time = _; yes_price; no_price } : Time_series.Point.t)
      ->
      let yes_bbo : Bbo.t = { bid = yes_price; ask = yes_price } in
      let no_bbo : Bbo.t = { bid = no_price; ask = no_price } in
      Pnl.apply_trade_report ~slug ~yes_bbo ~no_bbo pnl)
;;

let summarize pnl : Action_summary.t =
  { cash = Pnl.cash pnl
  ; realized_pnl = Pnl.realized_pnl pnl
  ; unrealized_pnl = Pnl.unrealized_pnl pnl
  ; inventory = Pnl.inventories pnl
  }
;;

let apply_action pnl (action : Action.t) ~time : Action_response.t =
  let { slug; size; side; contract_type; id = _ } : Action.t = action in
  match Pnl.mem pnl slug, Size.sign size with
  | false, _ ->
    Action_rejected
      { action
      ; time_stamp = time
      ; reason = [%string "market %{slug#Slug} is not in this simulation"]
      }
  | true, Neg ->
    Action_rejected
      { action
      ; time_stamp = time
      ; reason = "size must be non-negative; direction is carried by side"
      }
  | true, (Zero | Pos) ->
    Pnl.update_position pnl ~slug ~side ~contract_type ~size;
    Action_accepted { action; time_stamp = time }
;;

let run market_stubs (P ((module Bot), config)) =
  let start = Bot.Config.start config in
  let finish = Bot.Config.finish config in
  let interval = Bot.Config.interval config in
  let%bind.Deferred.Or_error () =
    Deferred.return (validate_simulation_window market_stubs ~start ~finish)
  in
  let%bind.Deferred.Or_error data_start, series_by_slug =
    load_aligned_series market_stubs ~start ~finish ~interval
  in
  let pnl =
    Pnl.create
      (List.map series_by_slug ~f:fst)
      (Bot.Config.initial_cash config)
  in
  let data =
    ref (Bot.init_data (Slug.Table.of_alist_exn series_by_slug) config)
  in
  let state = Bot.create_state config in
  let sim_start =
    Time_ns.add data_start (Bot.Config.sim_start_offset config)
  in
  Bot.on_start config state;
  List.iter (ticks_of_series series_by_slug) ~f:(fun (time, points) ->
    update_pnl_prices pnl points;
    Bot.State.forward_time state time;
    data := Bot.update_data config !data points;
    match Time_ns.( >= ) time sim_start with
    | false -> ()
    | true ->
      let actions = Bot.on_tick config state !data in
      let responses = List.map actions ~f:(apply_action pnl ~time) in
      Bot.on_response config state responses (summarize pnl));
  Deferred.Or_error.return (summarize pnl)
;;
