open! Core
open! Async
open! Types
open Arbitrage

(* A listing record with only the fields the bot reads; prices in cents. *)
let l1 ?yes_bid ?yes_ask ~venue ~id title =
  { L1_market_metadata.venue
  ; market_id = Market_id.of_string id
  ; title
  ; slug = Slug.of_string id
  ; event_slug = None
  ; series_ticker = None
  ; clob_token_id = None
  ; category = Crypto
  ; yes_bid = Option.map yes_bid ~f:Price.of_int_cents
  ; yes_ask = Option.map yes_ask ~f:Price.of_int_cents
  ; last_price = None
  ; volume = None
  ; volume_24h = None
  ; active = true
  ; created_time = Time_ns.epoch
  ; close_time = Time_ns.of_string "2030-01-01 00:00:00Z"
  }
;;

let candidate left right =
  { Matcher.Candidate.left = L1_market_metadata.to_market_stub left
  ; right = L1_market_metadata.to_market_stub right
  }
;;

let%expect_test "leg_of_l1 derives the NO ask from the YES bid and is \
                 honest about missing depth and missing quotes"
  =
  let quoted =
    l1 ~yes_bid:47 ~yes_ask:49 ~venue:Kalshi ~id:"K1" "BTC above 100k?"
  in
  let unquoted = l1 ~yes_ask:49 ~venue:Kalshi ~id:"K2" "BTC above 100k?" in
  print_s [%sexp (Bot.leg_of_l1 quoted : Detect.Leg.t option)];
  print_s [%sexp (Bot.leg_of_l1 unquoted : Detect.Leg.t option)];
  [%expect
    {|
    (((venue Kalshi) (market_id K1)
      (book
       ((yes_ask 49000000) (yes_ask_size 0) (no_ask 53000000) (no_ask_size 0)))))
    ()
    |}];
  return ()
;;

(* Two listings that read as the same market: Kalshi 49c YES ask / 47c YES
   bid, Polymarket 52c YES ask / 50c YES bid. Buying YES on Kalshi (49c) and
   NO on Polymarket (100c - 50c = 50c) costs 99c before fees. *)
let kalshi_l1 =
  l1 ~yes_bid:47 ~yes_ask:49 ~venue:Kalshi ~id:"K1" "Will BTC hit $100k?"
;;

let poly_l1 =
  l1 ~yes_bid:50 ~yes_ask:52 ~venue:Polymarket ~id:"P1" "will btc hit 100k"
;;

let markets = [ kalshi_l1; poly_l1 ]
let legs = List.filter_map markets ~f:Bot.leg_of_l1
let candidates = [ candidate kalshi_l1 poly_l1 ]

let%expect_test "scan: Exact reports nothing on depthless L1 data; Reckless \
                 surfaces the pre-fee edge"
  =
  let scan detect_mode =
    Bot.scan
      ~execution:{ Config.Execution.default with detect_mode }
      ~legs
      candidates
  in
  print_s [%sexp (scan Exact : Detect.opportunity list)];
  print_s [%sexp (scan Reckless : Detect.opportunity list)];
  [%expect
    {|
    ()
    (((yes ((venue Kalshi) (market_id K1) (price 49000000)))
      (no ((venue Polymarket) (market_id P1) (price 50000000))) (cost 99000000)
      (edge 1000000) (size 0)))
    |}];
  return ()
;;

let%expect_test "scan drops edges thinner than min_edge" =
  let scan min_edge =
    Bot.scan
      ~execution:
        { Config.Execution.default with
          detect_mode = Reckless
        ; min_edge = Price.of_int_cents min_edge
        }
      ~legs
      candidates
  in
  (* The Reckless edge above is exactly 1c: kept at a 1c floor, dropped at
     2c. *)
  print_s [%sexp (scan 1 : Detect.opportunity list)];
  print_s [%sexp (scan 2 : Detect.opportunity list)];
  [%expect
    {|
    (((yes ((venue Kalshi) (market_id K1) (price 49000000)))
      (no ((venue Polymarket) (market_id P1) (price 50000000))) (cost 99000000)
      (edge 1000000) (size 0)))
    ()
    |}];
  return ()
;;

let%expect_test "confirm_pairs in Text_only mode is pure matcher output" =
  let stubs l1s = List.map l1s ~f:L1_market_metadata.to_market_stub in
  let%bind confirmed =
    Bot.confirm_pairs
      ~matching:Config.Matching.default_text_only
      (stubs [ kalshi_l1 ])
      (stubs [ poly_l1 ])
  in
  print_s [%sexp (confirmed : Matcher.Candidate.t list)];
  [%expect
    {|
    (((left
       ((venue Kalshi) (market_id K1) (slug K1) (series_ticker ())
        (clob_token_id ()) (title "Will BTC hit $100k?") (category Crypto)
        (created_time (1970-01-01 00:00:00.000000000Z))
        (close_time (2030-01-01 00:00:00.000000000Z)) (volume ())))
      (right
       ((venue Polymarket) (market_id P1) (slug P1) (series_ticker ())
        (clob_token_id ()) (title "will btc hit 100k") (category Crypto)
        (created_time (1970-01-01 00:00:00.000000000Z))
        (close_time (2030-01-01 00:00:00.000000000Z)) (volume ())))))
    |}];
  return ()
;;

let%expect_test "confirm_pairs in Llm_assisted mode keeps only pairs the \
                 (here: seeded) adjudicator confirms"
  =
  let doubted =
    l1
      ~yes_bid:50
      ~yes_ask:52
      ~venue:Polymarket
      ~id:"P2"
      "will btc hit 100k in March"
  in
  let seed l1 verdict =
    Hashtbl.set
      Llm_matcher.For_testing.cache
      ~key:(Llm_matcher.For_testing.cache_key (candidate kalshi_l1 l1))
      ~data:verdict
  in
  seed poly_l1 { Llm_matcher.is_match = true; explanation = "same event" };
  seed
    doubted
    { Llm_matcher.is_match = false; explanation = "different deadline" };
  let stubs l1s = List.map l1s ~f:L1_market_metadata.to_market_stub in
  let%bind confirmed =
    Bot.confirm_pairs
      ~matching:Config.Matching.default_llm_assisted
      (stubs [ kalshi_l1 ])
      (stubs [ poly_l1; doubted ])
  in
  Hashtbl.clear Llm_matcher.For_testing.cache;
  print_s [%sexp (confirmed : Matcher.Candidate.t list)];
  [%expect
    {|
    (((left
       ((venue Kalshi) (market_id K1) (slug K1) (series_ticker ())
        (clob_token_id ()) (title "Will BTC hit $100k?") (category Crypto)
        (created_time (1970-01-01 00:00:00.000000000Z))
        (close_time (2030-01-01 00:00:00.000000000Z)) (volume ())))
      (right
       ((venue Polymarket) (market_id P1) (slug P1) (series_ticker ())
        (clob_token_id ()) (title "will btc hit 100k") (category Crypto)
        (created_time (1970-01-01 00:00:00.000000000Z))
        (close_time (2030-01-01 00:00:00.000000000Z)) (volume ())))))
    |}];
  return ()
;;

let%expect_test "candidates_of_approved: the tick prices exactly the pairs \
                 a human approved"
  =
  let db_path = "/tmp/arbiter-test-bot.db" in
  let%bind () =
    match%bind Sys.file_exists_exn db_path with
    | true -> Unix.unlink db_path
    | false -> return ()
  in
  ok_exn (Database.Database_exec.init_database db_path);
  let%bind () =
    Database.Database_exec.create_market_stub_table () >>| ok_exn
  in
  let%bind () =
    Database.Database_exec.create_pair_proposal_table () >>| ok_exn
  in
  let stub (l1 : L1_market_metadata.t) =
    L1_market_metadata.to_market_stub l1
  in
  let%bind () =
    Deferred.List.iter
      ~how:`Sequential
      [ stub kalshi_l1; stub poly_l1 ]
      ~f:(fun stub ->
        Database.Database_exec.insert_market_stub stub >>| ok_exn)
  in
  let propose_and_set status =
    let proposal =
      Pair_proposal.create
        ~left:(stub kalshi_l1).market_id
        ~right:(stub poly_l1).market_id
        ~score:0.9
        ~explanation:None
    in
    let%bind () = Database.Database_exec.propose_pair proposal >>| ok_exn in
    Database.Database_exec.set_pair_status
      ~left:proposal.left
      ~right:proposal.right
      status
    >>| ok_exn
  in
  (* Proposed but not approved: the tick must not touch it. *)
  let%bind () = propose_and_set Proposed in
  let%bind candidates = Bot.candidates_of_approved () >>| ok_exn in
  print_s [%sexp (List.length candidates : int)];
  (* Approved: now it prices. *)
  let%bind () = propose_and_set Approved in
  let%bind candidates = Bot.candidates_of_approved () >>| ok_exn in
  List.iter candidates ~f:(fun { Matcher.Candidate.left; right } ->
    print_endline [%string "%{left.title}  <->  %{right.title}"]);
  [%expect {|
    0
    Will BTC hit $100k?  <->  will btc hit 100k
    |}];
  return ()
;;

let%expect_test "orders_of_opportunity turns a hit into its two limit \
                 orders — YES here, NO there, at the asks the detector saw \
                 — and a missing stub is an error, not a guess"
  =
  let kalshi_stub = L1_market_metadata.to_market_stub kalshi_l1 in
  let poly_stub = L1_market_metadata.to_market_stub poly_l1 in
  let stubs =
    Market_id.Map.of_alist_exn
      [ kalshi_stub.market_id, kalshi_stub; poly_stub.market_id, poly_stub ]
  in
  let opportunity =
    { Detect.yes =
        { venue = Kalshi
        ; market_id = kalshi_stub.market_id
        ; price = Price.of_int_cents 49
        }
    ; no =
        { venue = Polymarket
        ; market_id = poly_stub.market_id
        ; price = Price.of_int_cents 50
        }
    ; cost = Price.of_int_cents 99
    ; edge = Price.of_int_cents 1
    ; size = Size.of_int 10
    }
  in
  let print_orders stubs =
    match Bot.orders_of_opportunity ~stubs opportunity with
    | Error error -> print_s [%message "no orders" ~_:(error : Error.t)]
    | Ok orders ->
      List.iter
        orders
        ~f:
          (fun
            { Execution.Order.market; contract; side; limit_price; size } ->
          print_s
            [%message
              ""
                ~market:(market.market_id : Market_id.t)
                ~venue:(market.venue : Venue.t)
                (contract : Contract_type.t)
                (side : Side.t)
                (limit_price : Price.t)
                (size : Size.t)])
  in
  print_orders stubs;
  print_orders (Map.remove stubs poly_stub.market_id);
  [%expect
    {|
    ((market K1) (venue Kalshi) (contract Yes) (side Buy) (limit_price 49000000)
     (size 10))
    ((market P1) (venue Polymarket) (contract No) (side Buy)
     (limit_price 50000000) (size 10))
    ("no orders"
     ("opportunity references a market with no stub"
      (entry ((venue Polymarket) (market_id P1) (price 50000000)))))
    |}];
  return ()
;;
