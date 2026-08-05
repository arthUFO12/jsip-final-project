open! Core
open! Async
open Types
open Arbitrage

(* The server half of the arbitrage pipeline: each function backs one
   {!Protocol} RPC by delegating to the same {!Arbitrage} modules the CLI
   verbs use — [Sweep] files proposals, [Review] lists and decides them, and
   one detection tick prices the approved set like {!Bot} would, without
   placing orders. Wire conversions (status mirror, float dollars) all live
   here, at the boundary. *)

let status_of_wire : Protocol.Pair_status.t -> Pair_proposal.Status.t
  = function
  | Proposed -> Proposed
  | Approved -> Approved
  | Rejected -> Rejected
;;

let status_to_wire : Pair_proposal.Status.t -> Protocol.Pair_status.t
  = function
  | Proposed -> Proposed
  | Approved -> Approved
  | Rejected -> Rejected
;;

let microcents_per_dollar = 100_000_000.

let dollars price =
  Float.of_int (Price.to_microcents price) /. microcents_per_dollar
;;

let card_of_listed ({ index; proposal; left; right } : Review.Listed.t)
  : Protocol.Pair_card.t
  =
  let title_and_venue stub market_id =
    match (stub : Market_stub.t option) with
    | Some stub -> stub.title, Venue.to_string stub.venue
    | None -> [%string "<no stub for %{market_id#Market_id}>"], "?"
  in
  let left_title, left_venue = title_and_venue left proposal.left in
  let right_title, right_venue = title_and_venue right proposal.right in
  { index
  ; left_title
  ; left_venue
  ; right_title
  ; right_venue
  ; score = proposal.score
  ; explanation = proposal.explanation
  ; status = status_to_wire proposal.status
  }
;;

let list_pairs status =
  let open Deferred.Or_error.Let_syntax in
  let%map listed = Review.list_by_status (status_of_wire status) in
  List.map listed ~f:card_of_listed
;;

let decide ({ tab; index; new_status } : Protocol.Decide_request.t) =
  let open Deferred.Or_error.Let_syntax in
  let%map decided =
    Review.decide
      ~from:(status_of_wire tab)
      ~index
      ~status:(status_of_wire new_status)
  in
  card_of_listed decided
;;

let auto_review ({ threshold } : Protocol.Auto_review_request.t) =
  let open Deferred.Or_error.Let_syntax in
  let%map { Review.Auto_review_summary.reviewed; approved; left_proposed } =
    Review.auto_review_proposed ~threshold
  in
  { Protocol.Auto_review_summary.reviewed; approved; left_proposed }
;;

let llm_review ({ api_key } : Protocol.Llm_review_request.t) =
  let open Deferred.Or_error.Let_syntax in
  let%map { Review.Llm_review_summary.reviewed
          ; approved
          ; rejected
          ; errored
          ; first_error
          }
    =
    Review.llm_review_proposed ?api_key ()
  in
  { Protocol.Llm_review_summary.reviewed
  ; approved
  ; rejected
  ; errored
  ; first_error
  }
;;

let sweep ({ threshold } : Protocol.Sweep_request.t) =
  let open Deferred.Or_error.Let_syntax in
  let%map { Sweep.markets_swept
          ; search_hits
          ; fresh_candidates = _
          ; proposed
          ; auto_rejected = _
          }
    =
    Sweep.sweep_full
      ~matching:(Text_only { threshold })
      ~max_adjudications:0
      ()
  in
  { Protocol.Sweep_summary.markets_swept; search_hits; proposed }
;;

(* The bot's own execution defaults decide what counts as tradable, so the
   page and the paper bot never disagree about a hit. *)
let execution_defaults = Config.Execution.default

(* The venue's own page for a market — the "actually go do it" link. Kalshi
   groups markets under a series page; Polymarket routes by slug. *)
let venue_url (stub : Market_stub.t) =
  match stub.venue with
  | Kalshi ->
    let series =
      match stub.series_ticker with
      | Some series -> Slug.to_string series
      | None ->
        (* A Kalshi ticker's leading segment is its series. *)
        List.hd_exn
          (String.split (Market_id.to_string stub.market_id) ~on:'-')
    in
    [%string "https://kalshi.com/markets/%{String.lowercase series}"]
  | Polymarket -> [%string "https://polymarket.com/market/%{stub.slug#Slug}"]
;;

let pair_key ({ left; right } : Matcher.Candidate.t) =
  let left = Market_id.to_string left.market_id in
  let right = Market_id.to_string right.market_id in
  match String.( <= ) left right with
  | true -> [%string "%{left}|%{right}"]
  | false -> [%string "%{right}|%{left}"]
;;

(* Price one split of a pair — buy YES on [yes], NO on [no] — into the wire
   card the page renders. [cost] carries the fees {!Detect} charges; the
   per-leg [ask] shown next to each venue is the raw book price. [acted] is
   stamped later, from the wallet. *)
let card_of_split
  ~pair_key
  ~(yes_stub : Market_stub.t)
  ~(no_stub : Market_stub.t)
  ~(yes : Detect.Leg.t)
  ~(no : Detect.Leg.t)
  =
  let cost, size = Detect.cost_and_size ~mode:Exact ~yes ~no in
  let cost = dollars cost in
  let edge = 1. -. cost in
  let size = Size.to_int size in
  { Protocol.Edge_card.pair_key
  ; yes =
      { venue = Venue.to_string yes.venue
      ; title = yes_stub.title
      ; ask = dollars yes.book.yes_ask
      ; url = venue_url yes_stub
      }
  ; no =
      { venue = Venue.to_string no.venue
      ; title = no_stub.title
      ; ask = dollars no.book.no_ask
      ; url = venue_url no_stub
      }
  ; cost
  ; edge
  ; size
  ; dollars = edge *. Float.of_int size
  ; tradable =
      Float.( >= ) edge (dollars execution_defaults.min_edge) && size > 0
  ; acted = false
  }
;;

let edge_card ~legs ({ Matcher.Candidate.left; right } as candidate) =
  let leg_of (stub : Market_stub.t) =
    List.find legs ~f:(fun (leg : Detect.Leg.t) ->
      Market_id.equal leg.market_id stub.market_id)
  in
  match leg_of left, leg_of right with
  | None, None | None, Some _ | Some _, None -> None
  | Some left_leg, Some right_leg ->
    let pair_key = pair_key candidate in
    let card_a =
      card_of_split
        ~pair_key
        ~yes_stub:left
        ~no_stub:right
        ~yes:left_leg
        ~no:right_leg
    in
    let card_b =
      card_of_split
        ~pair_key
        ~yes_stub:right
        ~no_stub:left
        ~yes:right_leg
        ~no:left_leg
    in
    Some (if Float.( <= ) card_a.cost card_b.cost then card_a else card_b)
;;

(* Book a tradable edge into the "for fun" wallet at the profit taking the
   whole opportunity would have locked in. Frozen (acted) rows are left alone
   by the upsert. *)
let book_edge (card : Protocol.Edge_card.t) =
  Database.Database_exec.upsert_wallet_entry
    { Wallet_entry.pair_key = card.pair_key
    ; summary =
        [%string
          "YES %{card.yes.venue}: %{card.yes.title}  +  NO \
           %{card.no.venue}: %{card.no.title}"]
    ; edge = card.edge
    ; size = card.size
    ; dollars = card.dollars
    ; acted = false
    ; acted_dollars = 0.
    }
;;

let wallet () =
  let open Deferred.Or_error.Let_syntax in
  let%map entries = Database.Database_exec.list_wallet_entries () in
  let paper =
    List.sum (module Float) entries ~f:(fun entry -> entry.dollars)
  in
  let acted =
    List.sum (module Float) entries ~f:(fun entry -> entry.acted_dollars)
  in
  { Protocol.Wallet.paper
  ; acted
  ; entries =
      List.map
        entries
        ~f:
          (fun
            { Wallet_entry.pair_key
            ; summary
            ; edge = _
            ; size = _
            ; dollars
            ; acted
            ; acted_dollars
            }
          ->
          { Protocol.Wallet_card.pair_key
          ; summary
          ; dollars
          ; acted
          ; acted_dollars
          })
  }
;;

let mark_acted pair_key =
  let open Deferred.Or_error.Let_syntax in
  let%bind () = Database.Database_exec.mark_wallet_acted ~pair_key in
  wallet ()
;;

(* The server's one live executor, installed at startup by [server.ml] iff it
   was launched with [-allow-live] and the credentials loaded. [None] = paper
   server: every execution affordance stays hidden. The raw credentials ride
   along for the one operation the executor doesn't cover: canceling a
   resting hedge remainder. *)
let live_state
  : (Execution.Executor.t * Execution.Kalshi_live.Credentials.t) option ref
  =
  ref None
;;

let enable_live credentials =
  live_state := Some (Execution.Executor.live credentials, credentials)
;;

(* Whether the operator launched with [-allow-live] — recorded at startup so
   the wallet-unlock RPC honors the same gate as env-credential startup: a
   browser can never talk a paper server into going live. *)
let live_allowed = ref false
let set_live_allowed allowed = live_allowed := allowed
let wallet_path () = Execution.Wallet_store.default_path ()

let trading_key_status () =
  let%map status = Execution.Wallet_store.status ~path:(wallet_path ()) () in
  let unlocked = Option.is_some !live_state in
  match status with
  | Execution.Wallet_store.Status.Not_connected ->
    Ok
      { Protocol.Trading_key.Status.connected = false
      ; unlocked
      ; key_hint = None
      ; production = false
      ; live_allowed = !live_allowed
      }
  | Execution.Wallet_store.Status.Connected { key_hint; production } ->
    Ok
      { Protocol.Trading_key.Status.connected = true
      ; unlocked
      ; key_hint = Some key_hint
      ; production
      ; live_allowed = !live_allowed
      }
;;

let connect_trading_key
  { Protocol.Trading_key.Connect_request.key_id
  ; private_key_pem
  ; passphrase
  ; production
  }
  =
  let open Deferred.Or_error.Let_syntax in
  let path = wallet_path () in
  let%bind () =
    Execution.Wallet_store.save
      ~path
      ~key_id
      ~private_key_pem
      ~passphrase
      ~production
      ()
  in
  (* Connecting also unlocks when the launch flag allows it — the user just
     typed the passphrase; asking again immediately would be pure friction.
     On a paper server the key is stored but stays locked, and the status
     tells the UI why. *)
  let%bind () =
    match !live_allowed with
    | false -> return ()
    | true ->
      let%bind credentials =
        Execution.Wallet_store.unlock ~path ~passphrase ()
      in
      enable_live credentials;
      return ()
  in
  trading_key_status ()
;;

let unlock_trading_key passphrase =
  let open Deferred.Or_error.Let_syntax in
  let%bind () =
    match !live_allowed with
    | true -> return ()
    | false ->
      Deferred.Or_error.error_string
        "this server was started without -allow-live, so it cannot trade; \
         restart it with the flag to unlock"
  in
  let%bind credentials =
    Execution.Wallet_store.unlock ~path:(wallet_path ()) ~passphrase ()
  in
  enable_live credentials;
  trading_key_status ()
;;

let lock_trading_key () =
  live_state := None;
  trading_key_status ()
;;

let forget_trading_key () =
  let open Deferred.Or_error.Let_syntax in
  live_state := None;
  let%bind () = Execution.Wallet_store.forget ~path:(wallet_path ()) () in
  trading_key_status ()
;;

let capability () =
  match !live_state with
  | None ->
    Deferred.Or_error.return
      { Protocol.Execution_capability.live = false
      ; reason =
          Some
            "server was not started with -allow-live (and Kalshi \
             credentials); connect and unlock a trading key on the Wallet \
             page of a -allow-live server"
      }
  | Some
      ((_ : Execution.Executor.t), (_ : Execution.Kalshi_live.Credentials.t))
    ->
    (match%map.Deferred Execution.Kill_switch.engaged () with
     | Some reason ->
       Ok
         { Protocol.Execution_capability.live = false
         ; reason = Some [%string "kill switch engaged: %{reason}"]
         }
     | None ->
       Ok { Protocol.Execution_capability.live = true; reason = None })
;;

(* Both stubs of a pair, split into the Kalshi (hedge) side and the manual
   side — exactly one leg must be Kalshi. *)
let hedge_stub_of_pair_key pair_key =
  let open Deferred.Or_error.Let_syntax in
  match String.lsplit2 pair_key ~on:'|' with
  | None ->
    Deferred.Or_error.error_s
      [%message "malformed pair key" (pair_key : string)]
  | Some (left, right) ->
    let%bind left =
      Database.Database_exec.find_pair_stub (Market_id.of_string left)
    in
    let%bind right =
      Database.Database_exec.find_pair_stub (Market_id.of_string right)
    in
    (match left, right with
     | Some left, Some right ->
       (match left.venue, right.venue with
        | Kalshi, Polymarket -> return left
        | Polymarket, Kalshi -> return right
        | (Kalshi | Polymarket), _ ->
          Deferred.Or_error.error_s
            [%message
              "pair does not have exactly one Kalshi leg to hedge on"
                (pair_key : string)])
     | None, Some _ | Some _, None | None, None ->
       Deferred.Or_error.error_s
         [%message
           "pair has missing leg snapshots; cannot hedge" (pair_key : string)])
;;

let hedge_contract ~manual_is_yes : Contract_type.t =
  match manual_is_yes with true -> No | false -> Yes
;;

(* Current ask and depth for one contract of the hedge market, from a fresh
   book read. *)
let hedge_quote (stub : Market_stub.t) ~(contract : Contract_type.t) =
  let open Deferred.Or_error.Let_syntax in
  let%map book = Market_data.Market_data_gateway.fetch_book stub in
  match contract with
  | Yes -> book.yes_ask, Size.to_int book.yes_ask_size
  | No -> book.no_ask, Size.to_int book.no_ask_size
;;

let cap_dollars price =
  Float.of_int (Price.to_microcents price) /. 100_000_000.
;;

let preflight ({ pair_key; manual_is_yes } : Protocol.Preflight_request.t) =
  let open Deferred.Or_error.Let_syntax in
  let%bind stub = hedge_stub_of_pair_key pair_key in
  let contract = hedge_contract ~manual_is_yes in
  let%bind ask, depth = hedge_quote stub ~contract in
  let%bind kill_switch = Deferred.ok (Execution.Kill_switch.engaged ()) in
  let%bind spent_today =
    Database.Database_exec.sum_trade_dollars_since (Rails.utc_day_start ())
  in
  let execution = execution_defaults in
  let day_room =
    Float.max 0. (cap_dollars execution.max_dollars_per_day -. spent_today)
  in
  let room =
    Float.min day_room (cap_dollars execution.max_dollars_per_order)
  in
  let ask_dollars = dollars ask in
  let by_caps =
    match Float.( > ) ask_dollars 0. with
    | true -> Int.of_float (room /. ask_dollars)
    | false -> 0
  in
  let hedgeable =
    match kill_switch with
    | Some (_ : string) -> 0
    | None -> Int.min depth by_caps
  in
  return
    { Protocol.Preflight.kill_switch
    ; hedge_title = stub.title
    ; hedge_is_yes = (match contract with Yes -> true | No -> false)
    ; ask_cents = Price.to_int_cents_exn ask
    ; depth
    ; caps_room_dollars = room
    ; hedgeable
    }
;;

(* Kalshi's error bodies, classified into the wire taxonomy. Substring
   matching is crude but the codes are stable words. *)
let classify_venue_error error =
  let text = String.lowercase (Error.to_string_hum error) in
  let has fragment = String.is_substring text ~substring:fragment in
  if has "insufficient_balance" || has "insufficient balance"
  then Protocol.Venue_error.Insufficient_funds
  else if has "rate limit" || has "too many requests" || has "429"
  then Rate_limited
  else if has "not_found" || has "closed" || has "inactive" || has "halted"
  then Market_closed_or_halted
  else if has "geo" || has "region" || has "restricted"
  then Geo_blocked
  else Other (Error.to_string_hum error)
;;

let log_manual_leg
  ~(stub : Market_stub.t)
  ~manual_venue
  ~manual_price_cents
  ~manual_count
  =
  Database.Database_exec.append_trade_log
    { at = Time_ns.now ()
    ; venue = [%string "%{manual_venue}(manual)"]
    ; market_id = stub.market_id
    ; action = "manual"
    ; client_order_id = None
    ; outcome = "self-reported by user; unverified"
    ; detail =
        [%string
          "assisted manual leg: %{manual_count#Int} @ \
           %{manual_price_cents#Int}c"]
    ; dollars = Float.of_int (manual_price_cents * manual_count) /. 100.
    }
;;

let hedge
  ({ pair_key
   ; nonce
   ; manual_venue
   ; manual_is_yes
   ; manual_price_cents
   ; manual_count
   ; max_hedge_price_cents
   } :
    Protocol.Hedge_request.t)
  =
  let open Deferred.Or_error.Let_syntax in
  match !live_state with
  | None ->
    return
      (Protocol.Hedge_result.Refused
         "server is not live-enabled; start it with -allow-live")
  | Some (executor, credentials) ->
    let%bind () =
      match manual_count > 0 && manual_price_cents > 0 with
      | true -> return ()
      | false ->
        Deferred.Or_error.error_s
          [%message
            "manual leg must have positive price and count"
              (manual_price_cents : int)
              (manual_count : int)]
    in
    let%bind stub = hedge_stub_of_pair_key pair_key in
    let contract = hedge_contract ~manual_is_yes in
    let%bind ask, depth = hedge_quote stub ~contract in
    let ask_cents = Price.to_int_cents_exn ask in
    (match ask_cents > max_hedge_price_cents with
     | true ->
       return
         (Protocol.Hedge_result.Refused
            [%string
              "slippage guard: ask moved to %{ask_cents#Int}c, above your \
               confirmed max of %{max_hedge_price_cents#Int}c"])
     | false ->
       let size = Int.min manual_count depth in
       (match size <= 0 with
        | true ->
          return
            (Protocol.Hedge_result.Refused
               "no depth at the ask right now; retry shortly")
        | false ->
          let order =
            { Execution.Order.market = stub
            ; contract
            ; side = Buy
            ; limit_price = ask
            ; size = Size.of_int size
            }
          in
          let client_order_id = [%string "hedge-%{nonce}"] in
          (match%bind.Deferred
             Rails.live_refusal ~execution:execution_defaults order
           with
           | Some reason ->
             let%bind () =
               Deferred.ok
                 (Rails.log_live_order
                    order
                    ~client_order_id
                    ~outcome:[%string "refused: %{reason}"]
                    ~dollars:0.)
             in
             return (Protocol.Hedge_result.Refused reason)
           | None ->
             (* The manual leg enters the audit trail first — the trail must
                show the whole pair even if the hedge then fails. *)
             let%bind () =
               log_manual_leg
                 ~stub
                 ~manual_venue
                 ~manual_price_cents
                 ~manual_count
             in
             (match%bind.Deferred
                Execution.Executor.place_order
                  ~client_order_id
                  executor
                  order
              with
              | Error error ->
                let%bind () =
                  Deferred.ok
                    (Rails.log_live_order
                       order
                       ~client_order_id
                       ~outcome:
                         [%string "error: %{Error.to_string_hum error}"]
                       ~dollars:0.)
                in
                return
                  (Protocol.Hedge_result.Failed
                     { error = classify_venue_error error
                     ; detail = Error.to_string_hum error
                     ; unhedged = manual_count
                     })
              | Ok fill ->
                let filled = Size.to_int fill.filled_size in
                let%bind () =
                  Deferred.ok
                    (Rails.log_live_order
                       order
                       ~client_order_id
                       ~outcome:[%string "accepted: filled %{filled#Int}"]
                       ~dollars:(Rails.order_dollars order))
                in
                (* A remainder resting on the book is not a hedge the user
                   can count on; cancel it so [unhedged] is definitive. *)
                let%bind () =
                  match filled < size, fill.venue_order_id with
                  | true, Some order_id ->
                    (match%map.Deferred
                       Execution.Kalshi_live.cancel_order
                         credentials
                         ~order_id
                     with
                     | Ok (_ : int) | Error (_ : Error.t) -> Ok ())
                  | false, _ | true, None -> return ()
                in
                let unhedged = manual_count - filled in
                let fee = dollars fill.fee in
                let hedge_price = dollars fill.price in
                let%bind () =
                  match filled > 0 with
                  | false -> return ()
                  | true ->
                    let realized =
                      ((1.
                        -. (Float.of_int manual_price_cents /. 100.)
                        -. hedge_price)
                       *. Float.of_int filled)
                      -. fee
                    in
                    Database.Database_exec.mark_wallet_acted_assisted
                      ~pair_key
                      ~dollars:realized
                in
                return
                  (Protocol.Hedge_result.Hedged
                     { price = hedge_price; count = filled; fee; unhedged })))))
;;

let scan () =
  let open Deferred.Or_error.Let_syntax in
  let%bind candidates = Bot.candidates_of_approved () in
  let%bind legs = Deferred.ok (Bot.fetch_legs candidates) in
  let edges = List.filter_map candidates ~f:(edge_card ~legs) in
  let%bind () =
    Deferred.Or_error.List.iter
      ~how:`Sequential
      (List.filter edges ~f:(fun edge -> edge.tradable))
      ~f:book_edge
  in
  (* Stamp each card with the wallet's verdict, so "I traded this" buttons
     don't reappear for pairs already banked. *)
  let%bind entries = Database.Database_exec.list_wallet_entries () in
  let acted_keys =
    String.Set.of_list
      (List.filter_map entries ~f:(fun (entry : Wallet_entry.t) ->
         match entry.acted with true -> Some entry.pair_key | false -> None))
  in
  let edges =
    List.map edges ~f:(fun edge ->
      { edge with acted = Set.mem acted_keys edge.pair_key })
  in
  return
    { Protocol.Scan_report.pairs = List.length candidates
    ; legs_priced = List.length legs
    ; edges
    ; tradable =
        List.count edges ~f:(fun edge -> edge.Protocol.Edge_card.tradable)
    }
;;
