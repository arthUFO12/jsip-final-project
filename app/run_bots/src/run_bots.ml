open! Core
open! Async
open Types

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
  let cash = Price.to_string_dollar summary.cash in
  let realized = Price.to_string_dollar summary.realized_pnl in
  let unrealized = Price.to_string_dollar summary.unrealized_pnl in
  print_endline
    [%string
      "final book: cash %{cash} | realized %{realized} | unrealized \
       %{unrealized}"];
  return ()
;;

let command =
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

let () = Command_unix.run command
