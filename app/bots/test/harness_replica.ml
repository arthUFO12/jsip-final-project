(* Shared test driver for {!Bots.Configurable}: replicates the
   {!Simulation.Harness} tick loop over a synthetic hourly price grid for one
   market with a real {!Pnl.t} — no network, deterministic output. The loop
   mirrors harness.ml: update pnl prices, forward time, update data, then
   (once the sim has started) on_tick / apply / on_response. *)

open! Core
open Types
open Market_data
open Bots

let slug = Slug.of_string "save-act"
let hour = Time_ns.Span.of_hr 1.
let start = Time_ns.of_string "2026-01-01 00:00:00Z"
let time_of_tick i = Time_ns.add start (Time_ns.Span.scale_int hour i)

let point ~time ~yes : Time_series.Point.t =
  let yes_price = Price.of_float_dollars yes in
  { time; yes_price; no_price = Price.( - ) Price.one yes_price }
;;

(* Mirrors [Harness.apply_action], including its reason strings. *)
let apply_action pnl (action : Action.t) ~time : Action_response.t =
  let { Action.slug; size; side; contract_type; id = _ } = action in
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

let summarize = Simulation.Harness.summarize

(* Run the bot over [yes_prices] (one per hourly tick, all for [slug]), with
   the first [warmup] ticks before the sim starts. *)
let drive ~rules ~yes_prices ~warmup ~initial_cash_dollars =
  let n = List.length yes_prices in
  let config =
    Configurable.Config.create
      ~id:0
      ~start
      ~finish:(time_of_tick (n - 1))
      ~interval:Hour
      ~sim_start_offset:(Time_ns.Span.scale_int hour warmup)
      ~initial_cash:
        (Price.of_int_cents (Float.to_int (initial_cash_dollars *. 100.)))
      ~slugs:[ slug ]
      ~rules
    |> Or_error.ok_exn
  in
  let points =
    List.mapi yes_prices ~f:(fun i yes -> point ~time:(time_of_tick i) ~yes)
  in
  let sim_start = time_of_tick warmup in
  let pnl =
    Pnl.create [ slug ] (Configurable.Bot.Config.initial_cash config)
  in
  let data =
    ref
      (Configurable.Bot.init_data
         (Slug.Table.of_alist_exn [ slug, points ])
         config)
  in
  let state = Configurable.Bot.create_state config in
  Configurable.Bot.on_start config state;
  List.iter points ~f:(fun pt ->
    let time = pt.time in
    let bbo price : Bbo.t = { bid = price; ask = price } in
    Pnl.apply_trade_report
      ~slug
      ~yes_bbo:(bbo pt.yes_price)
      ~no_bbo:(bbo pt.no_price)
      pnl;
    Configurable.Bot.State.forward_time state time;
    let tick_points = Slug.Table.of_alist_exn [ slug, pt ] in
    data := Configurable.Bot.update_data config !data tick_points;
    match Time_ns.( >= ) time sim_start with
    | false -> ()
    | true ->
      let actions = Configurable.Bot.on_tick config state !data in
      let responses = List.map actions ~f:(apply_action pnl ~time) in
      Configurable.Bot.on_response config state responses (summarize pnl));
  let summary = summarize pnl in
  print_endline
    [%string
      "final: cash %{Price.to_string_dollar summary.cash} | realized \
       %{Price.to_string_dollar summary.realized_pnl}"]
;;
