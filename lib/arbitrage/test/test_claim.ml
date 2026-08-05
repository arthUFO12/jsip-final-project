open! Core
open! Types
open Arbitrage

let default_close_time = Time_ns.of_string "2026-03-18 20:00:00Z"

let stub ?(close_time = default_close_time) title =
  { Market_stub.venue = Venue.Kalshi
  ; market_id = Market_id.of_string "M"
  ; slug = Slug.of_string "m"
  ; series_ticker = None
  ; clob_token_id = None
  ; title
  ; category = Category.Miscellaneous
  ; created_time = Time_ns.epoch
  ; close_time
  ; volume = None
  }
;;

let%expect_test "of_stub compiles each domain; ambiguity fails to compile" =
  List.iter
    [ (* rates *)
      "Will the Fed cut rates by 25 bps at the March meeting?"
    ; "Fed cut >25bps at the September 2026 meeting?"
    ; "Will the Fed hike rates by 0 bps in March?"
    ; "Will the Bank of Korea cut rates by 1-25bps at their September 2026 \
       meeting?"
    ; "Fed rate hike in 2026?"
    ; "Will the Fed cut rates at the next meeting?"
    ; (* statistics *)
      "Will Bitcoin hit $100,000 by June 30?"
    ; "Will ETH reach above $4500.00 by Jan 1, 2027 at 12:00AM?"
    ; (* awards, with and without the folded subtitle *)
      "Who will win the Nobel Peace Prize? (Donald Trump)"
    ; "Will Yulia Navalnaya win the Nobel Peace Prize in 2026?"
    ; "Who will win the Ballon d'Or in 2026? (Ousmane Dembele)"
    ; "Will Ousmane Dembélé win the 2026 Ballon d'Or?"
    ; (* championships *)
      "Will Manchester United win the English Premier League?"
    ; "Will Manchester United win the 2026-27 UEFA Champions League \
       Championship?"
    ; "Will Barrett Pfeiffer finish in 2 place in Big Brother Season 28?"
    ; (* fixtures *)
      "Will Gen.G Global Academy win map 1 in the Hanwha Life Esports \
       Challengers vs. Gen.G Global Academy match?"
    ; "KS Cracovia Krakow vs. Pogon Szczecin: O/U 2.5"
    ; "Orix Buffaloes vs. Tohoku Rakuten Golden Eagles"
    ; (* media ranks *)
      "Will The Bombing of Pan Am 103: Limited Series be Top Global Netflix \
       Show on Aug 3, 2026?"
    ; (* compound outcome: a parse failure, never a subject *)
      "Will Jimmy Butler stay with Golden State or retire? (Stays with \
       Golden State or Retires)"
    ]
    ~f:(fun title ->
      print_s
        [%message title ~_:(Claim.of_stub (stub title) : Claim.t option)]);
  [%expect
    {|
    ("Will the Fed cut rates by 25 bps at the March meeting?"
     (((domain Rates) (subject ((key fed) (resolved true))) (arena ())
       (outcome (Rate_move (Exactly -25))) (scope Whole_event)
       (period (((lo (2026-03-01)) (hi 2026-04-01)))))))
    ("Fed cut >25bps at the September 2026 meeting?"
     (((domain Rates) (subject ((key fed) (resolved true))) (arena ())
       (outcome (Rate_move (Less_than -25))) (scope Whole_event)
       (period (((lo (2026-09-01)) (hi 2026-10-01)))))))
    ("Will the Fed hike rates by 0 bps in March?"
     (((domain Rates) (subject ((key fed) (resolved true))) (arena ())
       (outcome (Rate_move (Exactly 0))) (scope Whole_event)
       (period (((lo (2026-03-01)) (hi 2026-04-01)))))))
    ("Will the Bank of Korea cut rates by 1-25bps at their September 2026 meeting?"
     (((domain Rates) (subject ((key bok) (resolved true))) (arena ())
       (outcome (Rate_move (Between -25 -1))) (scope Whole_event)
       (period (((lo (2026-09-01)) (hi 2026-10-01)))))))
    ("Fed rate hike in 2026?"
     (((domain Rates) (subject ((key fed) (resolved true))) (arena ())
       (outcome (Rate_move (Greater_than 0))) (scope Whole_event)
       (period (((lo (2026-01-01)) (hi 2027-01-01)))))))
    ("Will the Fed cut rates at the next meeting?" ())
    ("Will Bitcoin hit $100,000 by June 30?"
     (((domain Statistic) (subject ((key btc) (resolved true))) (arena ())
       (outcome (Threshold (bound (At_least 100000)) (style Touch)))
       (scope Whole_event) (period (((lo ()) (hi 2026-07-01)))))))
    ("Will ETH reach above $4500.00 by Jan 1, 2027 at 12:00AM?"
     (((domain Statistic) (subject ((key eth) (resolved true))) (arena ())
       (outcome (Threshold (bound (At_least 4500)) (style Touch)))
       (scope Whole_event) (period (((lo ()) (hi 2027-01-02)))))))
    ("Who will win the Nobel Peace Prize? (Donald Trump)"
     (((domain Award) (subject ((key "donald trump") (resolved true)))
       (arena (((key "nobel peace prize") (resolved true)))) (outcome Win)
       (scope Whole_event) (period ()))))
    ("Will Yulia Navalnaya win the Nobel Peace Prize in 2026?"
     (((domain Award) (subject ((key "yulia navalnaya") (resolved true)))
       (arena (((key "nobel peace prize") (resolved true)))) (outcome Win)
       (scope Whole_event) (period (((lo (2026-01-01)) (hi 2027-01-01)))))))
    ("Who will win the Ballon d'Or in 2026? (Ousmane Dembele)"
     (((domain Award) (subject ((key "ousmane dembele") (resolved true)))
       (arena (((key "ballon d or") (resolved true)))) (outcome Win)
       (scope Whole_event) (period (((lo (2026-01-01)) (hi 2027-01-01)))))))
    ("Will Ousmane Demb\195\169l\195\169 win the 2026 Ballon d'Or?"
     (((domain Award) (subject ((key "ousmane dembele") (resolved true)))
       (arena (((key "ballon d or") (resolved true)))) (outcome Win)
       (scope Whole_event) (period (((lo (2026-01-01)) (hi 2027-01-01)))))))
    ("Will Manchester United win the English Premier League?"
     (((domain Championship)
       (subject ((key "manchester united") (resolved false)))
       (arena (((key epl) (resolved true)))) (outcome Win) (scope Whole_event)
       (period ()))))
    ("Will Manchester United win the 2026-27 UEFA Champions League Championship?"
     (((domain Championship)
       (subject ((key "manchester united") (resolved false)))
       (arena (((key ucl) (resolved true)))) (outcome Win) (scope Whole_event)
       (period (((lo (2026-01-01)) (hi 2028-01-01)))))))
    ("Will Barrett Pfeiffer finish in 2 place in Big Brother Season 28?"
     (((domain Championship)
       (subject ((key "barrett pfeiffer") (resolved false)))
       (arena (((key "big brother") (resolved true)))) (outcome (Place 2))
       (scope Whole_event) (period ()))))
    ("Will Gen.G Global Academy win map 1 in the Hanwha Life Esports Challengers vs. Gen.G Global Academy match?"
     (((domain Fixture) (subject ((key "gen g global academy") (resolved false)))
       (arena
        (((key "gen g global academy hanwha life esports challengers")
          (resolved false))))
       (outcome Win) (scope (Leg (kind game) (index 1)))
       (period (((lo ()) (hi 2026-03-19)))))))
    ("KS Cracovia Krakow vs. Pogon Szczecin: O/U 2.5"
     (((domain Fixture)
       (subject ((key "cracovia krakow pogon szczecin") (resolved false)))
       (arena (((key "cracovia krakow pogon szczecin") (resolved false))))
       (outcome (Threshold (bound (Greater_than 2.5)) (style Terminal)))
       (scope Whole_event) (period (((lo ()) (hi 2026-03-19)))))))
    ("Orix Buffaloes vs. Tohoku Rakuten Golden Eagles" ())
    ("Will The Bombing of Pan Am 103: Limited Series be Top Global Netflix Show on Aug 3, 2026?"
     (((domain Media_rank)
       (subject ((key "bombing of pan am 103 limited series") (resolved false)))
       (arena (((key "netflix global") (resolved true)))) (outcome (Place 1))
       (scope Whole_event) (period (((lo ()) (hi 2026-08-04)))))))
    ("Will Jimmy Butler stay with Golden State or retire? (Stays with Golden State or Retires)"
     ())
    |}]
;;

let show_verdict left_title right_title =
  let claim title = Claim.of_stub (stub title) in
  match claim left_title, claim right_title with
  | Some left, Some right ->
    print_s
      [%message
        ""
          ~_:(Claim.verdict left right : Claim.Verdict.t)
          left_title
          right_title]
  | None, (Some _ | None) | Some _, None ->
    print_s [%message "did not compile" left_title right_title]
;;

let%expect_test "verdicts: each false-positive class dies on its field" =
  (* Wrong player, same award: subject. *)
  show_verdict
    "Who will win the Ballon d'Or in 2026? (Rodri)"
    "Will Pedri win the 2026 Ballon d'Or?";
  (* ICJ vs ICC: alias-resolved orgs disagree confidently. *)
  show_verdict
    "Who will win the Nobel Peace Prize? (International Court of Justice)"
    "Will the International Criminal Court win the Nobel Peace Prize in \
     2026?";
  (* Same club, different competition: arena. *)
  show_verdict
    "Will Manchester United win the English Premier League?"
    "Will Manchester United win the 2026-27 UEFA Champions League \
     Championship?";
  (* Same fixture, winner vs total: outcome — decided even though the team
     names are not in the alias table. *)
  show_verdict
    "Cracovia Krakow vs Pogon Szczecin Winner? (Cracovia Krakow)"
    "KS Cracovia Krakow vs. Pogon Szczecin: O/U 2.5";
  (* #1 vs #2 on the same chart: outcome. *)
  show_verdict
    "Will The Bombing of Pan Am 103: Limited Series be Top Global Netflix \
     Show on Aug 3, 2026?"
    "Will \"The Bombing of Pan Am 103: Limited Series\" be the #2 global \
     Netflix show on Aug 3, 2026?";
  (* Win vs finish-second: outcome. *)
  show_verdict
    "Will Barrett Pfeiffer finish in 2 place in Big Brother Season 28?"
    "Will Barrett Pfeiffer win Big Brother season 28?";
  [%expect
    {|
    ((Decided (relation Disjoint) (deciding "subject disagrees"))
     "Who will win the Ballon d'Or in 2026? (Rodri)"
     "Will Pedri win the 2026 Ballon d'Or?")
    ((Decided (relation Disjoint) (deciding "subject disagrees"))
     "Who will win the Nobel Peace Prize? (International Court of Justice)"
     "Will the International Criminal Court win the Nobel Peace Prize in 2026?")
    ((Decided (relation Disjoint) (deciding "arena disagrees"))
     "Will Manchester United win the English Premier League?"
     "Will Manchester United win the 2026-27 UEFA Champions League Championship?")
    ((Decided (relation Disjoint) (deciding "outcome disagrees"))
     "Cracovia Krakow vs Pogon Szczecin Winner? (Cracovia Krakow)"
     "KS Cracovia Krakow vs. Pogon Szczecin: O/U 2.5")
    ((Decided (relation Disjoint) (deciding "outcome disagrees"))
     "Will The Bombing of Pan Am 103: Limited Series be Top Global Netflix Show on Aug 3, 2026?"
     "Will \"The Bombing of Pan Am 103: Limited Series\" be the #2 global Netflix show on Aug 3, 2026?")
    ((Decided (relation Disjoint) (deciding "outcome disagrees"))
     "Will Barrett Pfeiffer finish in 2 place in Big Brother Season 28?"
     "Will Barrett Pfeiffer win Big Brother season 28?")
    |}]
;;

let%expect_test "verdicts: equivalences and implications" =
  (* Diacritics fold: Dembélé = Dembele. *)
  show_verdict
    "Who will win the Ballon d'Or in 2026? (Ousmane Dembele)"
    "Will Ousmane Dembélé win the 2026 Ballon d'Or?";
  (* Same meeting, same move, different words. *)
  show_verdict
    "Will the Federal Reserve Cut rates by 25bps at their September 2026 \
     meeting? (Cut 25bps)"
    "Will the Fed decrease interest rates by 25 bps after the September \
     2026 meeting?";
  (* Hike by 0 bps is a hold. *)
  show_verdict
    "Will the Federal Reserve Hike rates by 0bps at their September 2026 \
     meeting? (Fed maintains rate)"
    "Will there be no change in Fed interest rates after the September 2026 \
     meeting?";
  (* BoK band vs exact: nested bounds, one-sided. *)
  show_verdict
    "Will the Bank of Korea cut rates by 1-25bps at their September 2026 \
     meeting?"
    "Will the Bank of Korea cut rates by 25 bps at their September 2026 \
     meeting?";
  (* December meeting inside the 2026 year window: Implies. *)
  show_verdict
    "Will the Fed hike rates in December 2026?"
    "Fed rate hike in 2026?";
  (* Half-open windows: "before Jan 1, 2027" = "in 2026". *)
  show_verdict
    "Will BTC hit $150k before Jan 1, 2027?"
    "Will BTC hit $150k in 2026?";
  [%expect
    {|
    ((Decided (relation Equivalent) (deciding "all fields agree"))
     "Who will win the Ballon d'Or in 2026? (Ousmane Dembele)"
     "Will Ousmane Demb\195\169l\195\169 win the 2026 Ballon d'Or?")
    ((Decided (relation Equivalent) (deciding "all fields agree"))
     "Will the Federal Reserve Cut rates by 25bps at their September 2026 meeting? (Cut 25bps)"
     "Will the Fed decrease interest rates by 25 bps after the September 2026 meeting?")
    ((Decided (relation Equivalent) (deciding "all fields agree"))
     "Will the Federal Reserve Hike rates by 0bps at their September 2026 meeting? (Fed maintains rate)"
     "Will there be no change in Fed interest rates after the September 2026 meeting?")
    ((Decided (relation Implied_by) (deciding "all fields agree"))
     "Will the Bank of Korea cut rates by 1-25bps at their September 2026 meeting?"
     "Will the Bank of Korea cut rates by 25 bps at their September 2026 meeting?")
    ((Decided (relation Implies) (deciding "period disagrees"))
     "Will the Fed hike rates in December 2026?" "Fed rate hike in 2026?")
    ((Decided (relation Equivalent) (deciding "all fields agree"))
     "Will BTC hit $150k before Jan 1, 2027?" "Will BTC hit $150k in 2026?")
    |}]
;;

let%expect_test "verdicts: abstention is per pair, per field" =
  (* Same competition, one side names no season: period Unknown, so this pair
     — and only this pair — goes back to the string pipeline. *)
  show_verdict
    "Will Bayern Munich win the Champions League?"
    "Will Bayern Munich win the 2026-27 UEFA Champions League Championship?";
  (* Unresolved names that differ: Unknown, not a guessed verdict. *)
  show_verdict
    "Will Philadelphia win the 2026 Pro Baseball National League \
     Championship?"
    "Will Philadelphia Phillies win the 2026 National League Championship \
     Series?";
  [%expect
    {|
    ((Abstained (field period)) "Will Bayern Munich win the Champions League?"
     "Will Bayern Munich win the 2026-27 UEFA Champions League Championship?")
    ((Abstained (field subject))
     "Will Philadelphia win the 2026 Pro Baseball National League Championship?"
     "Will Philadelphia Phillies win the 2026 National League Championship Series?")
    |}]
;;

let%expect_test "blocking key: subject + window month + domain" =
  let key title =
    print_s
      [%sexp
        (Option.map (Claim.of_stub (stub title)) ~f:Claim.blocking_key
         : Claim.Blocking_key.t option)]
  in
  key "Will the FOMC cut rates by 25 bps in March?";
  key "Federal Reserve lowers interest rates by 25 bps in March?";
  key "Will Bitcoin hit $100k by March 31?";
  key "Who will win the Ballon d'Or in 2026? (Rodri)";
  [%expect
    {|
    (((subject fed) (period 2026-04) (domain Rates)))
    (((subject fed) (period 2026-04) (domain Rates)))
    (((subject btc) (period 2026-04) (domain Statistic)))
    (((subject rodri) (period 2027-01) (domain Award)))
    |}]
;;
