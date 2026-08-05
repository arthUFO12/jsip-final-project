(* The web app's server half: on startup it tops the sqlite database up to
   its target of Kalshi market stubs (current and historical) — purging the
   least interesting stored markets first when the targets are already met,
   so the catalog rotates instead of resetting — then serves the Bonsai
   client bundle over HTTP and the {!Protocol} RPCs over the same port's
   websocket upgrade. *)

open! Core
open! Async
open Types
open Market_data
open Database

let current_market_count = 150
let historical_market_count = 100

(* The two halves are targeted jointly: Kalshi's "closed" listing is full of
   markets whose close_time is still in the future, so rows fetched as
   historical routinely count as current once stored. Splitting the targets
   strictly by close_time would make the historical half unfillable. *)
let total_market_target = current_market_count + historical_market_count

(* When both halves are at target, this many rows rotate out per startup:
   live markets by lowest volume, historical ones by oldest close time. *)
let live_purge_count = 50
let historical_purge_count = 30

(* Kalshi's event listing has no pagination cursor, so the only way a refetch
   can contain markets we have never stored is to ask for a deep page. 200
   events is the endpoint's practical ceiling. *)
let fetch_events_limit = 200

(* Read-side bound, comfortably above the seed size. The listing query has no
   ORDER BY, so a limit below the row count would drop an arbitrary subset —
   which shows up as whole categories missing from the UI. *)
let market_read_limit = 1_000

let stored_ids stubs =
  Market_id.Set.of_list
    (List.map stubs ~f:(fun (stub : Market_stub.t) -> stub.market_id))
;;

(* When the database is full, evict [live_purge_count] +
   [historical_purge_count] rows so the top-up below has room to rotate in
   fresh markets. Returns the evicted ids so the top-up can prefer genuinely
   new markets over re-adding these. *)
let purge_when_full () =
  let open Deferred.Or_error.Let_syntax in
  let%bind current_count = Database_exec.count_current_market_stubs () in
  let%bind historical_count =
    Database_exec.count_historical_market_stubs ()
  in
  match current_count + historical_count >= total_market_target with
  | false -> return Market_id.Set.empty
  | true ->
    let%bind live =
      Database_exec.list_current_market_stubs market_read_limit
    in
    let live_victims =
      Stub_selection.live_purge_victims live ~count:live_purge_count
    in
    let%bind oldest =
      Database_exec.list_oldest_historical_market_stubs
        historical_purge_count
    in
    let historical_victims =
      List.map oldest ~f:(fun (stub : Market_stub.t) -> stub.market_id)
    in
    let victims = live_victims @ historical_victims in
    let%bind () = Database_exec.delete_market_stubs_by_ids victims in
    printf
      "purged %d live and %d historical market stubs\n"
      (List.length live_victims)
      (List.length historical_victims);
    return (Market_id.Set.of_list victims)
;;

(* Historical markets are inactive by definition, so unlike the bot flows
   nothing here filters on [active]. *)
let seed_split split ~purged_ids =
  let open Deferred.Or_error.Let_syntax in
  let%bind current_count = Database_exec.count_current_market_stubs () in
  let%bind historical_count =
    Database_exec.count_historical_market_stubs ()
  in
  (* The current half tops up to its own target; the historical (closed)
     fetch then fills whatever remains to [total_market_target], since its
     rows may land on either side of the close_time boundary. *)
  let name, closed, needed =
    match split with
    | `Current -> "current", false, current_market_count - current_count
    | `Historical ->
      ( "historical"
      , true
      , total_market_target - (current_count + historical_count) )
  in
  match needed <= 0 with
  | true ->
    printf "%s: target already met\n" name;
    return ()
  | false ->
    let%bind metadata =
      Market_data_gateway.fetch_l1_market_data
        ~events_limit:fetch_events_limit
        ~venue:Kalshi
        ~closed
        ~limit:total_market_target
        ()
    in
    let fetched = List.map metadata ~f:L1_market_metadata.to_market_stub in
    let%bind stored_current =
      Database_exec.list_current_market_stubs market_read_limit
    in
    let%bind stored_historical =
      Database_exec.list_historical_market_stubs market_read_limit
    in
    let chosen, readds =
      Stub_selection.select_new
        ~fetched
        ~existing_ids:(stored_ids (stored_current @ stored_historical))
        ~purged_ids
        ~needed
    in
    let%bind () = Database_exec.insert_market_stubs chosen in
    printf "%s: %d needed, %d inserted\n" name needed (List.length chosen);
    (match readds > 0 with
     | true ->
       printf
         "%s: %d of those re-add just-purged markets - the Kalshi listing \
          held too few new ones\n"
         name
         readds
     | false -> ());
    return ()
;;

let seed_database () =
  let open Deferred.Or_error.Let_syntax in
  let%bind purged_ids = purge_when_full () in
  let%bind () = seed_split `Current ~purged_ids in
  seed_split `Historical ~purged_ids
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
  (* Fetched once, before the server accepts requests: concurrent simulations
     read the cutoff, so it must never be re-written while serving. *)
  let%bind () = Fetch_time_series.update_kalshi_historical_cutoff () in
  let%bind () =
    match Fetch_time_series.kalshi_historical_cutoff () with
    | Some (_ : Time_ns.t) -> return ()
    | None ->
      Deferred.return
        (Or_error.error_string
           "kalshi historical cutoff was not set by the startup fetch")
  in
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
      "Top the market database up from Kalshi (rotating out stale markets \
       when full), then serve the client bundle and its RPCs"
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
let check_sim
  ~port
  ~slugs
  ~program
  ~variables
  ~lookback_days
  ~warmup_hours
  ~starting_cash_dollars
  ~allow_negative_cash
  ~basic_bots
  ~basic_trade_probability
  ~basic_max_size
  =
  let open Deferred.Or_error.Let_syntax in
  let uri = Uri.of_string [%string "ws://localhost:%{port#Int}/"] in
  let%bind connection = Rpc_websocket.Rpc.client uri in
  let request : Protocol.Sim_request.t =
    { slugs = List.map slugs ~f:Slug.of_string
    ; program
    ; variables
    ; interval = Hour
    ; lookback_days
    ; warmup_hours
    ; starting_cash_cents = starting_cash_dollars * 100
    ; allow_negative_cash
    ; basic_bots =
        Option.map basic_bots ~f:(fun count : Protocol.Basic_bots.t ->
          { count
          ; trade_probability = basic_trade_probability
          ; max_size = basic_max_size
          })
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
  (match List.last result.baseline_ticks with
   | None -> ()
   | Some { time_s = _; cash; realized; unrealized; portfolio_value } ->
     printf
       "baseline (%d ticks): cash $%.2f | realized $%.2f | unrealized $%.2f \
        | portfolio $%.2f\n"
       (List.length result.baseline_ticks)
       cash
       realized
       unrealized
       portfolio_value);
  (match result.pnl_percentile with
   | None -> ()
   | Some percentile -> printf "pnl percentile: %.0f\n" percentile);
  List.iter result.fills ~f:(fun fill ->
    print_s (Protocol.Fill.sexp_of_t fill));
  (match Protocol.Sim_result.final result with
   | None -> print_endline "no ticks simulated"
   | Some
       { time_s = _; cash; realized; unrealized; yes_prices = _; inventory }
     ->
     let inventory =
       List.map inventory ~f:(fun (slug, count) ->
         [%string " %{slug#Slug} %{count#Int}"])
       |> String.concat
     in
     printf
       "final: cash $%.2f | realized $%.2f | unrealized $%.2f | inventory:%s\n"
       cash
       realized
       unrealized
       inventory);
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
      and variables =
        flag
          "-variable"
          (listed string)
          ~doc:"DEF variable definition like 'lot = 2' (repeatable)"
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
      and starting_cash_dollars =
        flag
          "-starting-cash-dollars"
          (optional_with_default 100 int)
          ~doc:"INT the bot's opening cash (default 100)"
      and allow_negative_cash =
        flag
          "-allow-negative-cash"
          no_arg
          ~doc:" let trades overdraw cash (default: reject them)"
      and basic_bots =
        flag
          "-basic-bots"
          (optional int)
          ~doc:"INT also run this many random baseline bots (default: off)"
      and basic_trade_probability =
        flag
          "-basic-trade-probability"
          (optional_with_default 0.2 float)
          ~doc:"FLOAT baseline per-market trade chance (default 0.2)"
      and basic_max_size =
        flag
          "-basic-max-size"
          (optional_with_default 100 int)
          ~doc:"INT baseline max trade size (default 100)"
      in
      fun () ->
        check_sim
          ~port
          ~slugs
          ~program
          ~variables
          ~lookback_days
          ~warmup_hours
          ~starting_cash_dollars
          ~allow_negative_cash
          ~basic_bots
          ~basic_trade_probability
          ~basic_max_size]
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
