(* The web app's server half: on startup it seeds the sqlite database with
   Kalshi market stubs (current and historical), then serves the Bonsai
   client bundle over HTTP and the {!Protocol} RPCs over the same port's
   websocket upgrade. *)

open! Core
open! Async
open Types
open Market_data
open Database

let current_market_count = 100
let historical_market_count = 50

(* Read-side bound, comfortably above the seed size. The listing query has no
   ORDER BY, so a limit below the row count would drop an arbitrary subset —
   which shows up as whole categories missing from the UI. *)
let market_read_limit = 1_000

(* Historical markets are inactive by definition, so unlike the bot flows
   nothing here filters on [active]. *)
let seed_database () =
  let open Deferred.Or_error.Let_syntax in
  let fetch_stubs ~closed ~limit =
    let%bind metadata =
      Market_data_gateway.fetch_l1_market_data ~venue:Kalshi ~closed ~limit
    in
    return (List.map metadata ~f:L1_market_metadata.to_market_stub)
  in
  let%bind current = fetch_stubs ~closed:false ~limit:current_market_count in
  let%bind historical =
    fetch_stubs ~closed:true ~limit:historical_market_count
  in
  let%bind () = Database_exec.clear_market_stubs () in
  let%bind () = Database_exec.insert_market_stubs (current @ historical) in
  printf
    "seeded %d current and %d historical market stubs\n"
    (List.length current)
    (List.length historical);
  return ()
;;

let implementations =
  Rpc.Implementations.create_exn
    ~implementations:
      [ Rpc.Rpc.implement Protocol.get_markets (fun () () ->
          let%map stubs =
            Database_exec.list_current_market_stubs market_read_limit
          in
          Or_error.map stubs ~f:(List.map ~f:Protocol.Market_card.of_stub))
      ; Rpc.Rpc.implement Protocol.run_simulation (fun () request ->
          Sim_runner.run request)
      ]
    ~on_unknown_rpc:`Close_connection
    ~on_exception:Log_on_background_exn
;;

let http_handler ~client_js =
  Cohttp_static_handler.Single_page_handler.create_handler
    (Cohttp_static_handler.Single_page_handler.default_with_body_div
       ~div_id:"app")
    ~title:"Arbiter"
    ~assets:
      [ Cohttp_static_handler.Asset.local
          Cohttp_static_handler.Asset.Kind.javascript
          (Cohttp_static_handler.Asset.What_to_serve.file
             ~path:client_js
             ~relative_to:`Cwd)
      ]
    ~on_unknown_url:`Not_found
;;

let serve ~port ~db_name ~client_js =
  let open Deferred.Or_error.Let_syntax in
  let%bind () = Deferred.return (Database_exec.init_database db_name) in
  let%bind () = Database_exec.create_market_stub_table () in
  let%bind () = seed_database () in
  let handler = http_handler ~client_js in
  let%bind.Deferred (_ : (Socket.Address.Inet.t, int) Cohttp_async.Server.t) =
    Rpc_websocket.Rpc.serve
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ~implementations
      ~initial_connection_state:
        (fun
          ()
          (_ : Rpc_websocket.Rpc.Connection_initiated_from.t)
          (_ : Socket.Address.Inet.t)
          (_ : Rpc.Connection.t)
        -> ())
      ~http_handler:(fun () -> handler)
      ()
  in
  printf "serving http://localhost:%d\n" port;
  Deferred.never ()
;;

let serve_command =
  Command.async_or_error
    ~summary:
      "Seed the market database from Kalshi, then serve the client bundle \
       and its RPCs"
    [%map_open.Command
      let port =
        flag
          "-port"
          (optional_with_default 8080 int)
          ~doc:"INT port to listen on (default 8080)"
      and db_name =
        flag
          "-db"
          (optional_with_default "arbiter.db" string)
          ~doc:"FILE sqlite database file name (default arbiter.db)"
      and client_js =
        flag
          "-client-js"
          (optional_with_default
             "_build/default/app/client/bin/main.bc.js"
             string)
          ~doc:"PATH compiled client bundle (default from dune's _build)"
      in
      fun () -> serve ~port ~db_name ~client_js]
;;

(* Dispatches [get-markets] against a running server the same way the browser
   client does, so the websocket path is testable headlessly. *)
let check_rpc ~port =
  let open Deferred.Or_error.Let_syntax in
  let uri = Uri.of_string [%string "ws://localhost:%{port#Int}/"] in
  let%bind connection = Rpc_websocket.Rpc.client uri in
  let%bind cards =
    Rpc.Rpc.dispatch Protocol.get_markets connection ()
    |> Deferred.map ~f:Or_error.join
  in
  printf "get-markets returned %d market cards\n" (List.length cards);
  List.sort_and_group
    cards
    ~compare:(fun (left : Protocol.Market_card.t) right ->
      Category.compare left.category right.category)
  |> List.iter ~f:(fun group ->
    let category = (List.hd_exn group).Protocol.Market_card.category in
    printf !"  %{Category}: %d\n" category (List.length group));
  return ()
;;

let check_rpc_command =
  Command.async_or_error
    ~summary:
      "Dispatch get-markets against a running server and print a sample"
    [%map_open.Command
      let port =
        flag
          "-port"
          (optional_with_default 8080 int)
          ~doc:"INT port the server listens on (default 8080)"
      in
      fun () -> check_rpc ~port]
;;

(* Dispatches [run-simulation] the same way the bot builder does, so the
   whole parse -> fetch -> backtest pipeline is testable headlessly. *)
let check_sim ~port ~slugs ~program ~lookback_days ~warmup_hours =
  let open Deferred.Or_error.Let_syntax in
  let uri = Uri.of_string [%string "ws://localhost:%{port#Int}/"] in
  let%bind connection = Rpc_websocket.Rpc.client uri in
  let request : Protocol.Sim_request.t =
    { slugs = List.map slugs ~f:Slug.of_string
    ; program
    ; interval = Hour
    ; lookback_days
    ; warmup_hours
    }
  in
  let%bind result =
    Rpc.Rpc.dispatch Protocol.run_simulation connection request
    |> Deferred.map ~f:Or_error.join
  in
  printf
    "%d ticks, %d fills\n"
    (List.length result.ticks)
    (List.length result.fills);
  List.iter result.fills ~f:(fun fill ->
    print_s (Protocol.Fill.sexp_of_t fill));
  (match Protocol.Sim_result.final result with
   | None -> print_endline "no ticks simulated"
   | Some { time_s = _; cash; realized; unrealized; yes_prices = _ } ->
     printf
       "final: cash $%.2f | realized $%.2f | unrealized $%.2f\n"
       cash
       realized
       unrealized);
  return ()
;;

let check_sim_command =
  Command.async_or_error
    ~summary:
      "Dispatch run-simulation against a running server and print the result"
    [%map_open.Command
      let port =
        flag
          "-port"
          (optional_with_default 8080 int)
          ~doc:"INT port the server listens on (default 8080)"
      and slugs =
        flag
          "-slug"
          (one_or_more_as_list string)
          ~doc:"SLUG market to trade (repeatable, 1-4)"
      and program =
        flag "-program" (required string) ~doc:"TEXT bot program source"
      and lookback_days =
        flag
          "-lookback-days"
          (optional_with_default 14 int)
          ~doc:"INT window length (default 14)"
      and warmup_hours =
        flag
          "-warmup-hours"
          (optional_with_default 12 int)
          ~doc:"INT history-only prefix (default 12)"
      in
      fun () -> check_sim ~port ~slugs ~program ~lookback_days ~warmup_hours]
;;

let command =
  Command.group
    ~summary:"The Arbiter web app server"
    [ "serve", serve_command
    ; "check-rpc", check_rpc_command
    ; "check-sim", check_sim_command
    ]
;;

let () = Command_unix.run command
