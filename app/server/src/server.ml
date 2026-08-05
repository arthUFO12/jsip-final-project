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

(* The detail popup shows the market's whole life: hourly candles while the
   history is short enough to chart, daily once it isn't. *)
let market_detail_interval ~now (stub : Market_stub.t)
  : Time_series.Interval.t
  =
  let age = Time_ns.diff now stub.created_time in
  match Time_ns.Span.( < ) age (Time_ns.Span.of_day 30.) with
  | true -> Hour
  | false -> Day
;;

let get_market_detail slug : Protocol.Market_detail.t Deferred.Or_error.t =
  let open Deferred.Or_error.Let_syntax in
  let%bind stub =
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
       | Some (_ : Slug.t) -> return stub)
  in
  let now = Time_ns.now () in
  let%bind prices, volumes =
    Market_data_gateway.fetch_one_ticker_history
      stub
      ~start:stub.created_time
      ~finish:(Time_ns.min now stub.close_time)
      ~interval:(market_detail_interval ~now stub)
  in
  return
    ({ prices =
         List.map prices ~f:(fun (point : Time_series.Point.t) ->
           ( Protocol.epoch_seconds point.time
           , Price.to_dollar_float point.yes_price ))
     ; volumes =
         List.map volumes ~f:(fun (time, volume) ->
           Protocol.epoch_seconds time, volume)
     }
     : Protocol.Market_detail.t)
;;

let implementations =
  Rpc.Implementations.create_exn
    ~implementations:
      [ Rpc.Rpc.implement Protocol.get_markets (fun () () ->
          let%map stubs =
            Database_exec.list_current_market_stubs market_read_limit
          in
          Or_error.map stubs ~f:(List.map ~f:Protocol.Market_card.of_stub))
      ; Rpc.Rpc.implement Protocol.get_market_detail (fun () slug ->
          get_market_detail slug)
      ; Rpc.Rpc.implement Protocol.run_simulation (fun () request ->
          Sim_runner.run request)
      ; Rpc.Rpc.implement Protocol.get_pairs (fun () status ->
          Arb_runner.list_pairs status)
      ; Rpc.Rpc.implement Protocol.get_pair_counts (fun () () ->
          Arb_runner.pair_counts ())
      ; Rpc.Rpc.implement Protocol.decide_pair (fun () request ->
          Arb_runner.decide request)
      ; Rpc.Rpc.implement Protocol.llm_review_pairs (fun () request ->
          Arb_runner.llm_review request)
      ; Rpc.Rpc.implement Protocol.auto_review_pairs (fun () request ->
          Arb_runner.auto_review request)
      ; Rpc.Rpc.implement Protocol.run_sweep (fun () request ->
          Arb_runner.sweep request)
      ; Rpc.Rpc.implement Protocol.scan_edges (fun () () ->
          Arb_runner.scan ())
      ; Rpc.Rpc.implement Protocol.get_execution_capability (fun () () ->
          Arb_runner.capability ())
      ; Rpc.Rpc.implement Protocol.preflight_hedge (fun () request ->
          Arb_runner.preflight request)
      ; Rpc.Rpc.implement Protocol.execute_hedge (fun () request ->
          Arb_runner.hedge request)
      ; Rpc.Rpc.implement Protocol.get_wallet (fun () () ->
          Arb_runner.wallet ())
      ; Rpc.Rpc.implement Protocol.mark_acted (fun () pair_key ->
          Arb_runner.mark_acted pair_key)
      ; Rpc.Rpc.implement Protocol.get_trading_key (fun () () ->
          Arb_runner.trading_key_status ())
      ; Rpc.Rpc.implement Protocol.connect_trading_key (fun () request ->
          Arb_runner.connect_trading_key request)
      ; Rpc.Rpc.implement Protocol.unlock_trading_key (fun () passphrase ->
          Arb_runner.unlock_trading_key passphrase)
      ; Rpc.Rpc.implement Protocol.lock_trading_key (fun () () ->
          Arb_runner.lock_trading_key ())
      ; Rpc.Rpc.implement Protocol.forget_trading_key (fun () () ->
          Arb_runner.forget_trading_key ())
      ; Rpc.Rpc.implement Protocol.get_account (fun () () ->
          Arb_runner.account ())
      ; Rpc.Rpc.implement Protocol.trip_kill_switch (fun () reason ->
          Arb_runner.trip_kill_switch reason)
      ]
    ~on_unknown_rpc:`Close_connection
    ~on_exception:Log_on_background_exn
;;

let http_handler ~client_js ~client_css_dir =
  (* [tokens.css] must precede [app.css]: the component rules consume the
     custom properties the token sheet defines. Both load before the bundle. *)
  let css_asset file =
    Cohttp_static_handler.Asset.local
      Cohttp_static_handler.Asset.Kind.css
      (Cohttp_static_handler.Asset.What_to_serve.file
         ~path:(client_css_dir ^/ file)
         ~relative_to:`Cwd)
  in
  Cohttp_static_handler.Single_page_handler.create_handler
    (Cohttp_static_handler.Single_page_handler.default_with_body_div
       ~div_id:"app")
    ~title:"Arbiter"
    ~assets:
      [ css_asset "tokens.css"
      ; css_asset "app.css"
      ; Cohttp_static_handler.Asset.local
          Cohttp_static_handler.Asset.Kind.javascript
          (Cohttp_static_handler.Asset.What_to_serve.file
             ~path:client_js
             ~relative_to:`Cwd)
      ]
    ~on_unknown_url:`Not_found
;;

let serve
  ~port
  ~db_name
  ~client_js
  ~client_css_dir
  ~allow_live
  ~max_order_dollars
  ~max_day_dollars
  =
  let open Deferred.Or_error.Let_syntax in
  (* The web server's only live path (assisted hedges) exists solely behind
     this launch flag: a routine restart cannot go live by accident, and a
     paper server hides every execution affordance. Credentials come from
     either source — [KALSHI_*] env vars right now, or the Wallet page's
     encrypted key unlocked later; env credentials are optional so a
     [-allow-live] server can start clean and wait for the unlock. *)
  let%bind () =
    Deferred.return
      (Arb_runner.set_execution_caps ~max_order_dollars ~max_day_dollars)
  in
  printf
    "spending caps: $%.2f per order, $%.2f per UTC day\n"
    max_order_dollars
    max_day_dollars;
  Arb_runner.set_live_allowed allow_live;
  let%bind () =
    match allow_live with
    | false -> return ()
    | true ->
      (match%bind.Deferred
         Execution.Kalshi_live.Credentials.load_from_env ()
       with
       | Ok credentials ->
         Arb_runner.enable_live credentials;
         printf
           "LIVE EXECUTION ENABLED (host %s) - assisted hedges will move \
            real money\n"
           (Execution.Kalshi_live.Credentials.host credentials);
         return ()
       | Error (_ : Error.t) ->
         printf
           "live execution ALLOWED but locked - connect/unlock a trading \
            key on the Wallet page (no KALSHI_* env credentials found)\n";
         return ())
  in
  (* Caqti's sqlite URI reads a relative path as a host component, so anchor
     the file to the working directory first — same as the arbitrage CLI's
     store setup. *)
  let%bind cwd = Deferred.ok (Sys.getcwd ()) in
  let db_name =
    if Filename.is_absolute db_name then db_name else cwd ^/ db_name
  in
  let%bind () = Deferred.return (Database_exec.init_database db_name) in
  let%bind () = Database_exec.create_market_stub_table () in
  let%bind () = Database_exec.create_pair_proposal_table () in
  let%bind () = Database_exec.create_pair_stub_table () in
  let%bind () = Database_exec.create_arb_wallet_table () in
  let%bind () = Database_exec.create_trade_log_table () in
  let%bind () = Database_exec.create_arb_observation_table () in
  (* Before the seed's purge: rescue any pair legs still in the catalog into
     their snapshot table, or the rotation may evict them. *)
  let%bind () = Database_exec.backfill_pair_stubs () in
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
  let handler = http_handler ~client_js ~client_css_dir in
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
      and client_css_dir =
        flag
          "-client-css-dir"
          (optional_with_default "app/client/static" string)
          ~doc:
            "DIR directory holding tokens.css and app.css (default \
             app/client/static)"
      and max_order_dollars =
        flag
          "-max-order-dollars"
          (optional_with_default 25. float)
          ~doc:"DOLLARS per-order spending cap for live trading (default 25)"
      and max_day_dollars =
        flag
          "-max-day-dollars"
          (optional_with_default 100. float)
          ~doc:
            "DOLLARS per-UTC-day spending cap for live trading (default 100)"
      and allow_live =
        flag
          "-allow-live"
          no_arg
          ~doc:
            " enable real-money assisted hedges. Needs KALSHI_API_KEY_ID \
             and KALSHI_PRIVATE_KEY_FILE (production credentials); every \
             order runs inside the spending caps, behind the kill switch, \
             and lands in the audit log"
      in
      fun () ->
        serve
          ~port
          ~db_name
          ~client_js
          ~client_css_dir
          ~allow_live
          ~max_order_dollars
          ~max_day_dollars]
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

(* Dispatches the arbitrage RPCs the same way the Arbitrage page does, so the
   pipeline surface is testable headlessly: list two statuses, then one live
   edge scan of the approved pairs. *)
let check_arb ~port =
  let open Deferred.Or_error.Let_syntax in
  let uri = Uri.of_string [%string "ws://localhost:%{port#Int}/"] in
  let%bind connection = Rpc_websocket.Rpc.client uri in
  let%bind () =
    Deferred.Or_error.List.iter
      ~how:`Sequential
      Protocol.Pair_status.all
      ~f:(fun status ->
        let%bind pairs =
          Rpc.Rpc.dispatch Protocol.get_pairs connection status
          |> Deferred.map ~f:Or_error.join
        in
        printf
          "get-pairs %s: %d\n"
          (Protocol.Pair_status.name status)
          (List.length pairs);
        return ())
  in
  let%bind report =
    Rpc.Rpc.dispatch Protocol.scan_edges connection ()
    |> Deferred.map ~f:Or_error.join
  in
  printf
    "scan-edges: %d pair(s), %d legs priced, %d tradable\n"
    report.pairs
    report.legs_priced
    report.tradable;
  List.iter report.edges ~f:(fun edge ->
    printf
      "  YES %s @ %s ask $%.2f  +  NO %s @ %s ask $%.2f  =  cost $%.2f, \
       edge $%.2f%s\n"
      edge.yes.title
      edge.yes.venue
      edge.yes.ask
      edge.no.title
      edge.no.venue
      edge.no.ask
      edge.cost
      edge.edge
      (if edge.tradable then "  TRADABLE" else ""));
  return ()
;;

let check_arb_command =
  Command.async_or_error
    ~summary:
      "Dispatch the arbitrage RPCs against a running server: pair counts by \
       status, then one live edge scan"
    [%map_open.Command
      let port =
        flag
          "-port"
          (optional_with_default 8080 int)
          ~doc:"INT port the server listens on (default 8080)"
      in
      fun () -> check_arb ~port]
;;

(* Dispatches [get-market-detail] the way the markets-page popup does, so the
   history fetch is testable headlessly. *)
let check_detail ~port ~slug =
  let open Deferred.Or_error.Let_syntax in
  let uri = Uri.of_string [%string "ws://localhost:%{port#Int}/"] in
  let%bind connection = Rpc_websocket.Rpc.client uri in
  let%bind detail =
    Rpc.Rpc.dispatch Protocol.get_market_detail connection slug
    |> Deferred.map ~f:Or_error.join
  in
  printf
    "%d price points, %d volume points\n"
    (List.length detail.prices)
    (List.length detail.volumes);
  (match List.hd detail.prices, List.last detail.prices with
   | Some (_, first), Some (_, last) ->
     printf "first price $%.2f | last price $%.2f\n" first last
   | _ -> print_endline "no price history returned");
  (match List.last detail.volumes with
   | None -> ()
   | Some (_, volume) -> printf "last candle volume %.2f contracts\n" volume);
  return ()
;;

let check_detail_command =
  Command.async_or_error
    ~summary:
      "Dispatch get-market-detail against a running server and print a \
       summary"
    [%map_open.Command
      let port =
        flag
          "-port"
          (optional_with_default 8080 int)
          ~doc:"INT port the server listens on (default 8080)"
      and slug =
        flag "-slug" (required string) ~doc:"SLUG market to inspect"
      in
      fun () -> check_detail ~port ~slug:(Slug.of_string slug)]
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
    ; comparison =
        (match basic_bots with
         | None -> Protocol.Comparison.No_comparison
         | Some count ->
           Dumb_bots
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
  (match result.truncated with
   | false -> ()
   | true -> print_endline "note: lookback truncated to available history");
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

(* The live-testing verb: exercise the entire assisted-hedge path — pair
   resolution, preflight, rails, placement, reconciliation, audit — against
   Kalshi's DEMO environment by default. Mirrors the arbitrage CLI's
   check-trade, but goes through Arb_runner rather than the venue directly,
   so what it proves is the exact code path the browser's dialog uses. The
   manual (Polymarket) leg is pretend: only the Kalshi hedge is placed. *)
let check_hedge ~db ~pair_key ~count ~manual_price_cents ~real =
  let open Deferred.Or_error.Let_syntax in
  let%bind cwd = Deferred.ok (Sys.getcwd ()) in
  let db = if Filename.is_absolute db then db else cwd ^/ db in
  let%bind () = Deferred.return (Database_exec.init_database db) in
  let%bind () = Database_exec.create_market_stub_table () in
  let%bind () = Database_exec.create_pair_proposal_table () in
  let%bind () = Database_exec.create_pair_stub_table () in
  let%bind () = Database_exec.create_arb_wallet_table () in
  let%bind () = Database_exec.create_trade_log_table () in
  let%bind () = Database_exec.create_arb_observation_table () in
  let host =
    match real with
    | true -> Execution.Kalshi_live.live_host
    | false -> Execution.Kalshi_live.demo_host
  in
  (match real with
   | true ->
     printf "REAL MONEY: hedging on production; worst case is count x ask\n"
   | false -> printf "demo environment - pretend money\n");
  printf
    "note: this writes real rows to the audit log and wallet in %s; use a \
     scratch -db to keep your main wallet clean\n"
    db;
  let%bind credentials =
    Execution.Kalshi_live.Credentials.load_from_env ~host ()
  in
  Arb_runner.set_live_allowed true;
  Arb_runner.enable_live credentials;
  let%bind pair_key =
    match pair_key with
    | Some key -> return key
    | None ->
      (match%bind Arbitrage.Bot.candidates_of_approved () with
       | [] ->
         Deferred.Or_error.error_string
           "no approved pairs in the store - run a sweep and approve one \
            first, or pass -pair-key"
       | { left; right } :: (_ : Arbitrage.Matcher.Candidate.t list) ->
         let ids =
           List.sort
             ~compare:String.compare
             [ Market_id.to_string left.market_id
             ; Market_id.to_string right.market_id
             ]
         in
         return (String.concat ids ~sep:"|"))
  in
  printf "pair: %s\n" pair_key;
  let%bind preflight =
    Arb_runner.preflight
      { Protocol.Preflight_request.pair_key; manual_is_yes = true }
  in
  printf !"preflight: %{sexp: Protocol.Preflight.t}\n" preflight;
  match preflight.hedgeable <= 0 with
  | true ->
    printf
      "not hedgeable right now (kill switch, caps, or no depth) - stopping \
       before placement\n";
    return ()
  | false ->
    let count = Int.min count preflight.hedgeable in
    let now_ms =
      Time_ns.to_int_ns_since_epoch (Time_ns.now ()) / 1_000_000
    in
    let%bind result =
      Arb_runner.hedge
        { Protocol.Hedge_request.pair_key
        ; nonce = [%string "check-%{now_ms#Int}"]
        ; manual_venue = "Polymarket"
        ; manual_is_yes = true
        ; manual_price_cents
        ; manual_count = count
        ; max_hedge_price_cents = preflight.ask_cents + 2
        }
    in
    printf !"hedge result: %{sexp: Protocol.Hedge_result.t}\n" result;
    let%bind entries = Database_exec.list_trade_log 5 in
    printf "audit log (newest first):\n";
    List.iter entries ~f:(fun entry ->
      printf !"  %{sexp: Trade_log_entry.t}\n" entry);
    return ()
;;

let check_hedge_command =
  Command.async_or_error
    ~summary:
      "run the whole assisted-hedge path (preflight, rails, placement, \
       reconciliation, audit) against Kalshi's DEMO environment; the manual \
       Polymarket leg is pretend"
    [%map_open.Command
      let db =
        flag
          "-db"
          (optional_with_default "arbiter.db" string)
          ~doc:
            "FILE sqlite database with an approved pair (default arbiter.db)"
      and pair_key =
        flag
          "-pair-key"
          (optional string)
          ~doc:"KEY the pair to hedge (default: first approved pair)"
      and count =
        flag
          "-count"
          (optional_with_default 1 int)
          ~doc:"N contracts for the pretend manual leg (default 1)"
      and manual_price_cents =
        flag
          "-manual-price-cents"
          (optional_with_default 50 int)
          ~doc:"CENTS pretend fill price of the manual leg (default 50)"
      and real =
        flag
          "-real"
          no_arg
          ~doc:
            " REAL MONEY: hedge on production instead of demo. KALSHI_* env \
             vars must hold production credentials; the spending caps and \
             kill switch still apply."
      in
      fun () -> check_hedge ~db ~pair_key ~count ~manual_price_cents ~real]
;;

let command =
  Command.group
    ~summary:"The Arbiter web app server"
    [ "serve", serve_command
    ; "check-rpc", check_rpc_command
    ; "check-arb", check_arb_command
    ; "check-detail", check_detail_command
    ; "check-sim", check_sim_command
    ; "check-hedge", check_hedge_command
    ]
;;

let () = Command_unix.run command
