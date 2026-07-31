open! Core
open! Async
open Types
open Market_data

let print_summary
  ({ cash; realized_pnl; unrealized_pnl; inventory = _ } : Action_summary.t)
  =
  let cash = Price.to_string_dollar cash in
  let realized = Price.to_string_dollar realized_pnl in
  let unrealized = Price.to_string_dollar unrealized_pnl in
  print_endline
    [%string
      "final book: cash %{cash} | realized %{realized} | unrealized \
       %{unrealized}"]
;;

let run_basic ~id ~seed =
  let open Deferred.Or_error.Let_syntax in
  let%bind config, stubs = Bots.Basic.Config.create ~id ~seed in
  print_endline "trading:";
  List.iter stubs ~f:(fun stub ->
    print_endline [%string "  %{stub#Market_stub}"]);
  let%bind summary =
    Simulation.Harness.run
      stubs
      (Simulation.Harness.P ((module Bots.Basic.Bot), config))
  in
  print_summary summary;
  return ()
;;

let basic_command =
  Command.async_or_error
    ~summary:"Backtest the basic random bot on recent Kalshi history"
    [%map_open.Command
      let id =
        flag
          "-id"
          (optional_with_default 0 int)
          ~doc:"INT bot id (default 0)"
      and seed =
        flag
          "-seed"
          (optional_with_default 0 int)
          ~doc:
            "INT rng seed; same seed reproduces the same trades (default 0)"
      in
      fun () -> run_basic ~id ~seed]
;;

(* The interactive subcommand: ask how many markets, fetch that many stubs
   from Kalshi, show them, read a bot-language program from stdin until EOF,
   and backtest {!Bots.Configurable} on those markets' recent history. *)

let market_fetch_limit = 50
let probe_window = Time_ns.Span.of_day 21.
let interval = Time_series.Interval.Hour
let sim_start_offset = Time_ns.Span.of_hr 12.
let initial_cash = Price.of_int_cents 10_000 (* $100 *)

(* Re-prompts until the user types a positive integer. *)
let rec prompt_market_count stdin =
  print_endline "how many markets do you want to trade on?";
  match%bind.Deferred Reader.read_line stdin with
  | `Eof ->
    Deferred.Or_error.error_string
      "stdin closed before a market count was entered"
  | `Ok line ->
    (match Int.of_string_opt (String.strip line) with
     | Some count when count > 0 -> Deferred.Or_error.return count
     | Some _ | None ->
       print_endline "please enter a positive whole number";
       prompt_market_count stdin)
;;

let choose_stubs metadata ~count =
  let stubs =
    List.filter metadata ~f:(fun (market : L1_market_metadata.t) ->
      market.active && Option.is_some market.series_ticker)
    |> List.map ~f:L1_market_metadata.to_market_stub
  in
  let chosen = List.take stubs count in
  match List.length chosen = count with
  | true -> Ok chosen
  | false ->
    Or_error.error_s
      [%message
        "not enough tradeable kalshi markets"
          ~wanted:(count : int)
          ~found:(List.length chosen : int)]
;;

let read_program stdin ~slugs =
  print_endline "";
  print_endline
    "enter your strategy, one statement per line; finish with ctrl+D:";
  let%bind.Deferred text = Reader.contents stdin in
  Deferred.return (Parser.Parse.program text ~slugs)
;;

let run_interactive ~id =
  let open Deferred.Or_error.Let_syntax in
  let stdin = Lazy.force Reader.stdin in
  let%bind count = prompt_market_count stdin in
  let%bind metadata =
    Market_data_gateway.fetch_l1_market_data
      ~venue:Kalshi
      ~closed:false
      ~limit:market_fetch_limit
  in
  let%bind stubs = Deferred.return (choose_stubs metadata ~count) in
  let%bind () = Fetch_time_series.update_kalshi_historical_cutoff () in
  let probe_finish = Time_ns.now () in
  let probe_start = Time_ns.sub probe_finish probe_window in
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
  print_endline "your bot will trade on:";
  List.iter stubs ~f:(fun stub ->
    print_endline [%string "  %{stub#Market_stub}"]);
  let slugs = List.map stubs ~f:(fun (stub : Market_stub.t) -> stub.slug) in
  print_endline
    "tickers usable in your strategy (a bare ticker or PRICE <ticker> is \
     its current YES price; INVENTORY <ticker> is your position):";
  List.iter slugs ~f:(fun slug -> print_endline [%string "  %{slug#Slug}"]);
  let%bind rules = read_program stdin ~slugs in
  let%bind config =
    Deferred.return
      (Bots.Configurable.Config.create
         ~id
         ~start
         ~finish
         ~interval
         ~sim_start_offset
         ~initial_cash
         ~slugs
         ~rules)
  in
  let%bind summary =
    Simulation.Harness.run
      stubs
      (Simulation.Harness.P ((module Bots.Configurable.Bot), config))
  in
  print_summary summary;
  return ()
;;

let interactive_command =
  Command.async_or_error
    ~summary:
      "Script a bot in the bot language and backtest it on live Kalshi \
       markets"
    [%map_open.Command
      let id =
        flag
          "-id"
          (optional_with_default 0 int)
          ~doc:"INT bot id (default 0)"
      in
      fun () -> run_interactive ~id]
;;

let command =
  Command.group
    ~summary:"Backtest trading bots on recent Kalshi history"
    [ "basic", basic_command; "interactive", interactive_command ]
;;

let () = Command_unix.run command
