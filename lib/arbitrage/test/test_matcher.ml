open! Core
open! Types
open Arbitrage

(* A minimal stub with only the fields the matcher looks at (title,
   close_time, market_id for dedup). Everything else is filler — and every
   defaulted stub shares one close time so the veto never fires on filler
   data. *)
let default_close_time = Time_ns.of_string "2030-01-01 00:00:00Z"

let stub
  ?(close_time = default_close_time)
  ?(venue = Venue.Kalshi)
  ?(category = Category.Miscellaneous)
  ~id
  title
  =
  { Market_stub.venue
  ; market_id = Market_id.of_string id
  ; slug = Slug.of_string id
  ; series_ticker = None
  ; clob_token_id = None
  ; title
  ; category
  ; created_time = Time_ns.epoch
  ; close_time
  ; volume = None
  }
;;

let candidate left right = { Matcher.Candidate.left; right }

let print_candidates candidates =
  List.iter candidates ~f:(fun { Matcher.Candidate.left; right } ->
    print_endline [%string "%{left.title}  <->  %{right.title}"])
;;

let%expect_test "normalize lowercases, drops punctuation, squeezes spaces" =
  List.iter
    [ "Will BTC hit $100k?"
    ; "will btc hit 100k"
    ; "  Fed    cuts rates -- March?!  "
    ]
    ~f:(fun title ->
      print_endline
        [%string "%{title} -> %{Matcher.For_testing.normalize title}"]);
  [%expect
    {|
    Will BTC hit $100k? -> will btc hit 100k
    will btc hit 100k -> will btc hit 100k
      Fed    cuts rates -- March?!   -> fed cuts rates march
    |}]
;;

let%expect_test "aliases fold venue-specific names to one spelling" =
  List.iter
    [ "Will Bitcoin hit $100k?"; "Ethereum above 5000"; "Federal rate cut" ]
    ~f:(fun title ->
      print_endline
        [%string "%{title} -> %{Matcher.For_testing.normalize title}"]);
  (* "Bitcoin"/"BTC" share no trigrams as raw strings, but block together and
     score as identical once aliased. *)
  let lefts = [ stub ~id:"K1" "Bitcoin higher?" ] in
  let rights = [ stub ~id:"P1" "BTC higher?" ] in
  print_candidates (Matcher.For_testing.block lefts rights);
  printf
    "%.2f\n"
    (Matcher.For_testing.jaccard "Bitcoin higher?" "BTC higher?");
  [%expect
    {|
    Will Bitcoin hit $100k? -> will btc hit 100k
    Ethereum above 5000 -> eth above 5000
    Federal rate cut -> fed rate cut
    Bitcoin higher?  <->  BTC higher?
    1.00
    |}]
;;

let%expect_test "trigrams of a title; short strings become one gram" =
  print_s [%sexp (Matcher.For_testing.trigrams "BTC up!" : String.Set.t)];
  print_s [%sexp (Matcher.For_testing.trigrams "No" : String.Set.t)];
  print_s [%sexp (Matcher.For_testing.trigrams "" : String.Set.t)];
  [%expect {|
    (" up" btc "c u" "tc ")
    (no)
    ()
    |}]
;;

let%expect_test "jaccard: 1.0 identical, high for paraphrases, 0.0 disjoint" =
  let show left right =
    printf
      "%.2f  %s / %s\n"
      (Matcher.For_testing.jaccard left right)
      left
      right
  in
  show "Will BTC hit $100k?" "will btc hit 100k";
  show "Will BTC hit 100k by June?" "BTC to hit $100k in June";
  show "Fed cuts rates in March" "Aliens land on the moon";
  show "" "anything";
  [%expect
    {|
    1.00  Will BTC hit $100k? / will btc hit 100k
    0.42  Will BTC hit 100k by June? / BTC to hit $100k in June
    0.00  Fed cuts rates in March / Aliens land on the moon
    0.00   / anything
    |}]
;;

let%expect_test "block pairs only markets sharing a title tag" =
  let lefts =
    [ stub ~id:"K1" "Will BTC hit 100k?"
    ; stub ~id:"K2" "Fed cuts rates in March"
    ]
  in
  let rights =
    [ stub ~id:"P1" "Bitcoin above 100k"
    ; stub ~id:"P2" "BTC to hit $100,000"
    ; stub ~id:"P3" "Rate cut by the Fed in March?"
    ; stub ~id:"P4" "Aliens land on the White House lawn"
    ]
  in
  print_candidates (Matcher.For_testing.block lefts rights);
  [%expect
    {|
    Will BTC hit 100k?  <->  Bitcoin above 100k
    Will BTC hit 100k?  <->  BTC to hit $100,000
    Fed cuts rates in March  <->  Rate cut by the Fed in March?
    |}]
;;

let%expect_test "block emits a pair once even when several tags overlap" =
  let lefts = [ stub ~id:"K1" "Will BTC hit 100k by June?" ] in
  let rights = [ stub ~id:"P1" "BTC to hit 100k in June" ] in
  print_candidates (Matcher.For_testing.block lefts rights);
  [%expect {| Will BTC hit 100k by June?  <->  BTC to hit 100k in June |}]
;;

let%expect_test "veto rejects conflicting numbers, keeps shared ones" =
  let show_veto left_title right_title =
    let result =
      Matcher.For_testing.veto
        (candidate (stub ~id:"K1" left_title) (stub ~id:"P1" right_title))
    in
    match result with
    | None ->
      print_endline [%string "kept:   %{left_title} / %{right_title}"]
    | Some reason ->
      print_endline
        [%string "vetoed: %{left_title} / %{right_title} (%{reason})"]
  in
  show_veto "BTC above 100k this year" "BTC above 200k this year";
  show_veto "BTC above $100,000 this year" "BTC above 100k this year";
  show_veto "Fed cuts rates" "Fed cuts rates";
  [%expect
    {|
    vetoed: BTC above 100k this year / BTC above 200k this year (titles disagree on numbers: [100] vs [200])
    kept:   BTC above $100,000 this year / BTC above 100k this year
    kept:   Fed cuts rates / Fed cuts rates
    |}]
;;

let%expect_test "veto rejects close times more than a day apart" =
  let time string = Time_ns.of_string string in
  let show_veto left_time right_time =
    let result =
      Matcher.For_testing.veto
        (candidate
           (stub ~id:"K1" ~close_time:left_time "Fed cuts rates")
           (stub ~id:"P1" ~close_time:right_time "Fed cuts rates"))
    in
    match result with
    | None -> print_endline "kept"
    | Some reason -> print_endline [%string "vetoed (%{reason})"]
  in
  show_veto (time "2026-03-01 00:00:00Z") (time "2026-06-01 00:00:00Z");
  show_veto (time "2026-03-01 00:00:00Z") (time "2026-03-01 18:00:00Z");
  [%expect {|
    vetoed (close times 92d apart)
    kept
    |}]
;;

let%expect_test "find_candidates under each Config.Matching mode" =
  let lefts =
    [ stub ~id:"K1" "Will BTC hit $100k by June 30?"
    ; stub ~id:"K2" "Will ETH hit 5000 this year?"
    ; stub ~id:"K3" "Fed decision in March"
    ]
  in
  let rights =
    [ stub ~id:"P1" "Will BTC hit 100k by June 30?"
      (* near-identical title: survives both modes *)
    ; stub ~id:"P2" "Will ETH hit 10000 this year?"
      (* similar text, conflicting numbers: only the veto catches it *)
    ; stub ~id:"P3" "March Madness winner announced"
      (* blocks with K3 via "march" but scores below even the loose 0.1 *)
    ]
  in
  let run mode =
    print_s [%sexp (mode : Config.Matching.t)];
    Matcher.find_candidates
      ~threshold:(Config.Matching.threshold mode)
      ~apply_veto:(Config.Matching.apply_veto mode)
      lefts
      rights
    |> List.iter ~f:(fun candidate ->
      printf
        "  %.2f  %s  <->  %s\n"
        (Matcher.For_testing.score candidate)
        candidate.left.title
        candidate.right.title)
  in
  (* Text-only: the pipeline has the final word, so it keeps only the pair it
     is sure of. *)
  run Config.Matching.default_text_only;
  (* LLM-assisted: a loose funnel with the veto off, so the ambiguous ETH
     pair survives for {!Llm_matcher} to adjudicate. *)
  run Config.Matching.default_llm_assisted;
  [%expect
    {|
    (Text_only (threshold 0.5))
      1.00  Will BTC hit $100k by June 30?  <->  Will BTC hit 100k by June 30?
    (Llm_assisted (threshold 0.1))
      1.00  Will BTC hit $100k by June 30?  <->  Will BTC hit 100k by June 30?
      0.21  Will BTC hit $100k by June 30?  <->  Will ETH hit 10000 this year?
      0.13  Will ETH hit 5000 this year?  <->  Will BTC hit 100k by June 30?
      0.79  Will ETH hit 5000 this year?  <->  Will ETH hit 10000 this year?
    |}]
;;

let%expect_test "category blocking: known-different categories never pair; \
                 Miscellaneous blocks nothing"
  =
  (* Identical titles, so tags and score would pair them — only the category
     gate separates these. *)
  let kalshi category =
    stub ~id:"K1" ~category "Will the price of gold hit $3000?"
  in
  let poly category =
    stub
      ~id:"P1"
      ~venue:Polymarket
      ~category
      "Will the price of gold hit $3000?"
  in
  let show ~left ~right =
    let candidates =
      Matcher.For_testing.block [ kalshi left ] [ poly right ]
    in
    print_endline
      [%string
        "%{left#Category} vs %{right#Category}: %{List.length \
         candidates#Int} candidate(s)"]
  in
  show ~left:Commodities ~right:Commodities;
  show ~left:Commodities ~right:Finance;
  show ~left:Commodities ~right:Miscellaneous;
  show ~left:Miscellaneous ~right:Finance;
  [%expect
    {|
    Commodities vs Commodities: 1 candidate(s)
    Commodities vs Finance: 0 candidate(s)
    Commodities vs Miscellaneous: 1 candidate(s)
    Miscellaneous vs Finance: 1 candidate(s)
    |}]
;;
