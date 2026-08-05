open! Core
open! Async
open! Types

let leg_of_l1 (l1 : L1_market_metadata.t) : Detect.Leg.t option =
  match l1.yes_ask, l1.yes_bid with
  | Some yes_ask, Some yes_bid ->
    Some
      { Detect.Leg.venue = l1.venue
      ; market_id = l1.market_id
      ; book =
          { yes_ask
          ; yes_ask_size = Size.zero
          ; no_ask = Price.( - ) Price.one yes_bid
          ; no_ask_size = Size.zero
          }
      }
  | None, Some _ | Some _, None | None, None -> None
;;

let confirm_candidates ~matching candidates =
  if not (Config.Matching.use_llm matching)
  then return (List.map candidates ~f:(fun candidate -> candidate, None))
  else
    Deferred.List.filter_map ~how:`Sequential candidates ~f:(fun candidate ->
      match%map Llm_matcher.adjudicate candidate with
      | Ok { is_match = true; explanation } ->
        Some (candidate, Some explanation)
      | Ok { is_match = false; explanation = _ } -> None
      | Error error ->
        (* One bad pair must not kill the sweep; skip it loudly. *)
        Core.eprint_s
          [%message
            "adjudication failed; skipping pair"
              (candidate : Matcher.Candidate.t)
              (error : Error.t)];
        None)
;;

let confirm_pairs ~matching lefts rights =
  let candidates =
    Matcher.find_candidates
      ~threshold:(Config.Matching.threshold matching)
      ~apply_veto:(Config.Matching.apply_veto matching)
      lefts
      rights
  in
  confirm_candidates ~matching candidates
  >>| List.map ~f:(fun (candidate, (_ : string option)) -> candidate)
;;

(* Books for every distinct market a candidate touches, straight from the
   venues' order-book endpoints. A market whose book can't be fetched (or has
   an empty side) is skipped with a note — its pairs simply won't price this
   sweep. *)
let fetch_legs candidates =
  let stubs =
    List.concat_map candidates ~f:(fun { Matcher.Candidate.left; right } ->
      [ left; right ])
    |> List.dedup_and_sort
         ~compare:
           (Comparable.lift Market_id.compare ~f:(fun stub ->
              stub.Market_stub.market_id))
  in
  Deferred.List.filter_map
    ~how:`Sequential
    stubs
    ~f:(fun (stub : Market_stub.t) ->
      match%map Market_data.Market_data_gateway.fetch_book stub with
      | Ok book ->
        Some
          { Detect.Leg.venue = stub.venue; market_id = stub.market_id; book }
      | Error error ->
        Core.eprint_s
          [%message
            "book fetch failed; skipping market"
              (stub.market_id : Market_id.t)
              (error : Error.t)];
        None)
;;

let scan ~(execution : Config.Execution.t) ~legs candidates =
  let legs =
    List.map legs ~f:(fun (leg : Detect.Leg.t) -> leg.market_id, leg)
    |> Market_id.Map.of_alist_reduce ~f:(fun first _duplicate -> first)
  in
  List.filter_map candidates ~f:(fun { Matcher.Candidate.left; right } ->
    let%bind.Option left = Map.find legs left.market_id in
    let%bind.Option right = Map.find legs right.market_id in
    Detect.find ~mode:execution.detect_mode left right)
  |> List.filter ~f:(fun { Detect.edge; _ } ->
    Price.( >= ) edge execution.min_edge)
  |> List.map ~f:(fun (opportunity : Detect.opportunity) ->
    { opportunity with
      size = Size.min opportunity.size execution.stake_per_opportunity
    })
;;

(* The approved pairs, joined back to their stubs. Matching already happened
   in the sweep and a human already signed off; the tick only prices. A pair
   whose stubs are missing is broken data — note and skip. *)
let candidates_of_approved () =
  let open Deferred.Or_error.Let_syntax in
  let%bind approved =
    Database.Database_exec.list_pair_proposals_by_status Approved
  in
  Deferred.Or_error.List.filter_map
    ~how:`Sequential
    approved
    ~f:(fun { left; right; score = _; explanation = _; status = _ } ->
      let%map left = Database.Database_exec.find_pair_stub left
      and right = Database.Database_exec.find_pair_stub right in
      match left, right with
      | Some left, Some right -> Some { Matcher.Candidate.left; right }
      | (None, Some _ | Some _, None | None, None) as stubs ->
        Core.eprint_s
          [%message
            "approved pair has missing stubs; skipping"
              (stubs : Market_stub.t option * Market_stub.t option)];
        None)
;;

let scan_once ~(config : Config.t) =
  let open Deferred.Or_error.Let_syntax in
  let%bind candidates = candidates_of_approved () in
  let%bind legs = Deferred.ok (fetch_legs candidates) in
  return (scan ~execution:config.execution ~legs candidates)
;;

let report opportunity =
  print_s
    [%message "arbitrage opportunity" ~_:(opportunity : Detect.opportunity)]
;;

(* Both legs of an opportunity as concrete orders: buy YES here, buy NO
   there, each at the ask the detector saw (so a book that has since moved
   against us cannot fill worse than priced) and at the opportunity's size. *)
let orders_of_opportunity ~stubs (opportunity : Detect.opportunity) =
  let order (entry : Detect.Entry.t) ~contract =
    match Map.find stubs entry.market_id with
    | Some market ->
      Ok
        { Execution.Order.market
        ; contract
        ; side = Buy
        ; limit_price = entry.price
        ; size = opportunity.size
        }
    | None ->
      Or_error.error_s
        [%message
          "opportunity references a market with no stub"
            (entry : Detect.Entry.t)]
  in
  Or_error.both
    (order opportunity.yes ~contract:Contract_type.Yes)
    (order opportunity.no ~contract:Contract_type.No)
  |> Or_error.map ~f:(fun (yes, no) -> [ yes; no ])
;;

(* $1000 of pretend cash; enough that paper fills fail on book reality, not
   on the bankroll. *)
let paper_starting_cash = Price.of_int_cents 100_000

(* The one place the paper/live fork happens. Paper fills against a re-read
   of the live book — real depth, real fees, and legging risk if the book
   moved since detection. Live loads Kalshi credentials from the environment,
   so a misconfigured live run dies at startup, not on the first hit. *)
let executor_of_trading (trading : Config.Trading.t) =
  match trading with
  | Paper ->
    Deferred.Or_error.return
      (Execution.Executor.paper
         (Execution.Simulator.create
            ~fill_model:Against_live_book
            ~starting_cash:paper_starting_cash))
  | Live ->
    let%map credentials =
      Execution.Kalshi_live.Credentials.load_from_env ()
    in
    Or_error.map credentials ~f:Execution.Executor.live
;;

let execute executor ~(execution : Config.Execution.t) ~stubs opportunity =
  report opportunity;
  match orders_of_opportunity ~stubs opportunity with
  | Error error ->
    Core.eprint_s
      [%message "cannot build orders; skipping hit" (error : Error.t)];
    return ()
  | Ok orders ->
    let live = Execution.Executor.is_live executor in
    (* Stable per attempt: a future retry of the same leg must re-send the
       same id, so the venue can deduplicate. *)
    let batch_ms =
      Time_ns.to_int_ns_since_epoch (Time_ns.now ()) / 1_000_000
    in
    let client_order_id ~leg =
      [%string "arbiter-%{batch_ms#Int}-%{leg#Int}"]
    in
    (* Sequential, aborting on the first failure or refusal: if the first leg
       doesn't fill there is nothing to hedge, so the second must not be
       sent. A failure after a fill leaves us one-sided — that is legging
       risk, and it must be loud. *)
    let rec place ~leg = function
      | [] -> return ()
      | order :: rest ->
        let client_order_id = client_order_id ~leg in
        let refusal =
          match live with
          | false -> return None
          | true -> Rails.live_refusal ~execution order
        in
        (match%bind refusal with
         | Some reason ->
           Core.eprint_s
             [%message
               "live order refused; aborting remaining legs"
                 (reason : string)
                 (order : Execution.Order.t)
                 ~legs_not_sent:(List.length rest : int)];
           (match live with
            | true ->
              Rails.log_live_order
                order
                ~client_order_id
                ~outcome:[%string "refused: %{reason}"]
                ~dollars:0.
            | false -> return ())
         | None ->
           (match%bind
              Execution.Executor.place_order ~client_order_id executor order
            with
            | Ok fill ->
              print_s [%message "filled" ~_:(fill : Execution.Fill.t)];
              let%bind () =
                match live with
                | true ->
                  Rails.log_live_order
                    order
                    ~client_order_id
                    ~outcome:"accepted"
                    ~dollars:(Rails.order_dollars order)
                | false -> return ()
              in
              place ~leg:(leg + 1) rest
            | Error error ->
              Core.eprint_s
                [%message
                  "order failed; aborting remaining legs - position may be \
                   one-sided"
                    (order : Execution.Order.t)
                    (error : Error.t)
                    ~legs_not_sent:(List.length rest : int)];
              (match live with
               | true ->
                 Rails.log_live_order
                   order
                   ~client_order_id
                   ~outcome:[%string "error: %{Error.to_string_hum error}"]
                   ~dollars:0.
               | false -> return ())))
    in
    place ~leg:0 orders
;;

(* The stubs behind the candidates, keyed by market id, so an opportunity
   (which only carries ids) can be joined back to the routing data an order
   needs. *)
let stub_map candidates =
  List.concat_map candidates ~f:(fun { Matcher.Candidate.left; right } ->
    [ left; right ])
  |> List.map ~f:(fun (stub : Market_stub.t) -> stub.market_id, stub)
  |> Market_id.Map.of_alist_reduce ~f:(fun first (_ : Market_stub.t) ->
    first)
;;

(* A quiet market must still show a pulse: when nothing clears [min_edge],
   re-price with a zero floor (pure — the books are already fetched) and say
   how close the best pair came. *)
let heartbeat ~(config : Config.t) ~legs candidates =
  let thin =
    scan
      ~execution:{ config.execution with min_edge = Price.zero }
      ~legs
      candidates
  in
  let best =
    List.max_elt
      thin
      ~compare:
        (Comparable.lift Price.compare ~f:(fun (o : Detect.opportunity) ->
           o.edge))
  in
  match best with
  | None ->
    print_s
      [%message
        "tick: no positive edge"
          ~pairs:(List.length candidates : int)
          ~legs_priced:(List.length legs : int)]
  | Some { edge; size; _ } ->
    print_s
      [%message
        "tick: best edge is below min_edge"
          ~edge:(Price.to_string_dollar edge : string)
          ~size:(size : Size.t)
          ~min_edge:
            (Price.to_string_dollar config.execution.min_edge : string)]
;;

let tick ~(config : Config.t) ~executor =
  let open Deferred.Or_error.Let_syntax in
  let%bind candidates = candidates_of_approved () in
  let%bind legs = Deferred.ok (fetch_legs candidates) in
  let opportunities = scan ~execution:config.execution ~legs candidates in
  (match opportunities with
   | [] -> heartbeat ~config ~legs candidates
   | _ :: _ -> ());
  Deferred.ok
    (Deferred.List.iter
       ~how:`Sequential
       opportunities
       ~f:
         (execute
            executor
            ~execution:config.execution
            ~stubs:(stub_map candidates)))
;;

let run ~config =
  match Config.validate config with
  | Error _ as error -> return error
  | Ok config ->
    let open Deferred.Or_error.Let_syntax in
    let%bind executor = executor_of_trading config.trading in
    let rec loop () =
      let%bind () =
        Deferred.ok
          (match%map.Deferred tick ~config ~executor with
           | Ok () -> ()
           | Error error ->
             Core.eprint_s
               [%message "scan failed; will retry" (error : Error.t)])
      in
      let%bind () =
        Deferred.ok (Clock_ns.after config.execution.poll_interval)
      in
      loop ()
    in
    loop ()
;;
