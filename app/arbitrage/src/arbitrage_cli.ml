open! Core
open! Async
open! Types
open Arbitrage

(* The command-line face of the arbitrage pipeline. [sweep] files pair
   proposals with the review gate, [review] lets a human approve or reject
   them, [run] prices the approved set on a timer (paper unless -live) —
   those share one sqlite store — [compare] prints the claim and text
   matchers' verdicts side by side without touching the store, and
   [check-trade] round-trips one tiny order on Kalshi's demo environment to
   prove the live plumbing. *)

let init_store ~db =
  let open Deferred.Or_error.Let_syntax in
  let%bind cwd = Deferred.ok (Sys.getcwd ()) in
  let db = if Filename.is_absolute db then db else cwd ^/ db in
  let%bind () = Deferred.return (Database.Database_exec.init_database db) in
  let%bind () = Database.Database_exec.create_market_stub_table () in
  let%bind () = Database.Database_exec.create_pair_proposal_table () in
  let%bind () = Database.Database_exec.create_pair_stub_table () in
  let%bind () = Database.Database_exec.create_trade_log_table () in
  (* Rescue pair legs still in the catalog into their snapshot table — the
     web server's seed purges the catalog on every startup. *)
  Database.Database_exec.backfill_pair_stubs ()
;;

let db_flag =
  Command.Param.flag
    "-db"
    (Command.Param.optional_with_default "arbiter.db" Command.Param.string)
    ~doc:"FILE sqlite database (default arbiter.db)"
;;

let sweep_command =
  Command.async_or_error
    ~summary:
      "search Polymarket for likely twins of liquid Kalshi markets and file \
       them for review"
    (let%map_open.Command db = db_flag
     and markets =
       flag
         "-markets"
         (optional_with_default 25 int)
         ~doc:"N highest-volume Kalshi markets to sweep from (default 25)"
     and use_llm =
       flag
         "-use-llm"
         no_arg
         ~doc:
           " adjudicate candidates with the LLM before proposing (needs \
            ANTHROPIC_API_KEY)"
     and full =
       flag
         "-full"
         no_arg
         ~doc:
           " backstop mode: page through every open market on both venues \
            and match the full cross product instead of searching (slow; \
            run occasionally)"
     and max_llm =
       flag
         "-max-llm"
         (optional_with_default 100 int)
         ~doc:
           "N with -full -use-llm: adjudicate at most N fresh pairs this \
            run (default 100)"
     and threshold =
       flag
         "-threshold"
         (optional float)
         ~doc:
           "X override the title-similarity cutoff (default 0.5, or 0.1 \
            with -use-llm)"
     in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind () = init_store ~db in
       let matching : Config.Matching.t =
         match use_llm, threshold with
         | true, None -> Config.Matching.default_llm_assisted
         | false, None -> Config.Matching.default_text_only
         | true, Some threshold -> Llm_assisted { threshold }
         | false, Some threshold -> Text_only { threshold }
       in
       let%bind summary =
         if full
         then Sweep.sweep_full ~matching ~max_adjudications:max_llm ()
         else Sweep.sweep_once ~matching ~markets_to_sweep:markets ()
       in
       print_s [%sexp (summary : Sweep.summary)];
       return ())
;;

let status_arg =
  Command.Arg_type.create (fun status ->
    Pair_proposal.Status.of_string (String.capitalize status))
;;

let review_command =
  Command.async_or_error
    ~summary:
      "list pairs by status (default proposed), or approve/reject a \
       proposed pair by its listed number"
    (let%map_open.Command db = db_flag
     and approve =
       flag "-approve" (optional int) ~doc:"N approve the Nth listed pair"
     and reject =
       flag "-reject" (optional int) ~doc:"N reject the Nth listed pair"
     and status =
       flag
         "-status"
         (optional status_arg)
         ~doc:"STATUS list proposed|approved|rejected pairs instead"
     in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind () = init_store ~db in
       let decide index (status : Pair_proposal.Status.t) =
         let%bind decided = Review.decide ~from:Proposed ~index ~status in
         printf !"%{sexp: Pair_proposal.Status.t}: " status;
         print_endline (Review.Listed.to_display_string decided);
         return ()
       in
       let list (status : Pair_proposal.Status.t) =
         let%bind summary = Review.summary () in
         let { Review.Summary.proposed; approved; rejected } = summary in
         printf
           "arbitrage found so far: %d pair(s) — %d approved, %d awaiting \
            review, %d rejected\n"
           (Review.Summary.total summary)
           approved
           proposed
           rejected;
         let%bind listed = Review.list_by_status status in
         printf
           !"%d %{sexp: Pair_proposal.Status.t} pair(s), most likely first \
             — highest match score at the top:\n"
           (List.length listed)
           status;
         List.iter listed ~f:(fun entry ->
           print_endline (Review.Listed.to_display_string entry));
         return ()
       in
       match approve, reject, status with
       | Some _, Some _, _ ->
         Deferred.Or_error.error_string "pass -approve or -reject, not both"
       | Some _, None, Some _ | None, Some _, Some _ ->
         Deferred.Or_error.error_string
           "-status only lists; it cannot combine with -approve/-reject"
       | Some index, None, None -> decide index Approved
       | None, Some index, None -> decide index Rejected
       | None, None, status -> list (Option.value status ~default:Proposed))
;;

let run_command =
  Command.async_or_error
    ~summary:
      "price the approved pairs on a timer and report edges (paper unless \
       -live)"
    (let%map_open.Command db = db_flag
     and reckless =
       flag
         "-reckless"
         no_arg
         ~doc:
           " ignore fees and depth when pricing (never tradable; see docs)"
     and live =
       flag
         "-live"
         no_arg
         ~doc:
           " send real Kalshi orders instead of paper fills. Needs \
            KALSHI_API_KEY_ID and KALSHI_PRIVATE_KEY_FILE in the \
            environment; every order runs inside the config's spending caps \
            and behind the kill switch (TRADING_DISABLED env var or a \
            trading.disabled file), and lands in the audit log"
     in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind () = init_store ~db in
       let config =
         { Config.default with
           trading = (if live then Live else Paper)
         ; execution =
             { Config.Execution.default with
               detect_mode = (if reckless then Reckless else Exact)
             }
         }
       in
       Bot.run ~config)
;;

(* Phase 1's definition of done, as a verb: one full order lifecycle against
   Kalshi's demo environment — place a resting limit order, read its status,
   cancel it, read it again — with every step in the audit log. Never touches
   the production host. *)
let check_trade_command =
  Command.async_or_error
    ~summary:
      "place, poll, and cancel a tiny order on Kalshi's DEMO environment \
       (demo credentials in KALSHI_* env vars; no real money unless -real)"
    (let%map_open.Command db = db_flag
     and ticker =
       flag "-ticker" (required string) ~doc:"TICKER a market to probe"
     and price_cents =
       flag
         "-price-cents"
         (optional_with_default 1 int)
         ~doc:"CENTS limit price (default 1, so the order rests unfilled)"
     and count =
       flag
         "-count"
         (optional_with_default 1 int)
         ~doc:"N contracts (default 1)"
     and real =
       flag
         "-real"
         no_arg
         ~doc:
           " REAL MONEY: run the same lifecycle against production instead \
            of demo. The KALSHI_* env vars must hold production \
            credentials. Worst case is price-cents x count if the resting \
            order fills before the cancel."
     in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind () = init_store ~db in
       let%bind () =
         match%bind.Deferred Execution.Kill_switch.engaged () with
         | Some reason ->
           Deferred.Or_error.error_s
             [%message "kill switch engaged; refusing" (reason : string)]
         | None -> return ()
       in
       let host, venue_label =
         match real with
         | true -> Execution.Kalshi_live.live_host, "Kalshi"
         | false -> Execution.Kalshi_live.demo_host, "Kalshi(demo)"
       in
       (match real with
        | true ->
          printf
            "REAL MONEY canary: %d contract(s) at %dc on %s — worst case \
             $%.2f\n"
            count
            price_cents
            ticker
            (Float.of_int (price_cents * count) /. 100.)
        | false -> ());
       let%bind credentials =
         Execution.Kalshi_live.Credentials.load_from_env ~host ()
       in
       let market_id = Market_id.of_string ticker in
       let order =
         { Execution.Order.market =
             { venue = Kalshi
             ; market_id
             ; slug = Slug.of_string ticker
             ; series_ticker = None
             ; clob_token_id = None
             ; title = "check-trade probe"
             ; category = Miscellaneous
             ; created_time = Time_ns.epoch
             ; close_time = Time_ns.epoch
             ; volume = None
             }
         ; contract = Yes
         ; side = Buy
         ; limit_price = Price.of_int_cents price_cents
         ; size = Size.of_int count
         }
       in
       let log ~action ~client_order_id ~outcome ~dollars =
         Database.Database_exec.append_trade_log
           { at = Time_ns.now ()
           ; venue = venue_label
           ; market_id
           ; action
           ; client_order_id
           ; outcome
           ; detail = [%string "check-trade %{ticker}"]
           ; dollars
           }
       in
       let now_ms =
         Time_ns.to_int_ns_since_epoch (Time_ns.now ()) / 1_000_000
       in
       let client_order_id = [%string "check-trade-%{now_ms#Int}"] in
       let%bind fill =
         Execution.Kalshi_live.place_order ~client_order_id credentials order
       in
       printf !"placed: %{sexp: Execution.Fill.t}\n" fill;
       let%bind () =
         log
           ~action:"place"
           ~client_order_id:(Some client_order_id)
           ~outcome:"accepted"
           ~dollars:(Float.of_int (price_cents * count) /. 100.)
       in
       match fill.venue_order_id with
       | None ->
         Deferred.Or_error.error_string
           "venue returned no order id; cannot poll or cancel"
       | Some order_id ->
         (* The venue's read side lags the matching engine by a second or two
            — a just-placed order reads as not_found at first. *)
         let rec poll_status ~attempts_left =
           match%bind.Deferred
             Execution.Kalshi_live.order_status credentials ~order_id
           with
           | Ok status -> return status
           | Error error when attempts_left > 0 ->
             Core.eprint_s
               [%message
                 "order not readable yet; retrying"
                   (attempts_left : int)
                   (error : Error.t)];
             let%bind.Deferred () =
               Clock_ns.after (Time_ns.Span.of_sec 2.)
             in
             poll_status ~attempts_left:(attempts_left - 1)
           | Error _ as error -> Deferred.return error
         in
         let%bind status = poll_status ~attempts_left:5 in
         printf "status after place: %s\n" status;
         let%bind reduced_by =
           Execution.Kalshi_live.cancel_order credentials ~order_id
         in
         printf "canceled, %d contract(s) removed from the book\n" reduced_by;
         let%bind () =
           log
             ~action:"cancel"
             ~client_order_id:None
             ~outcome:[%string "canceled: reduced_by %{reduced_by#Int}"]
             ~dollars:0.
         in
         let%bind status =
           Execution.Kalshi_live.order_status credentials ~order_id
         in
         printf "status after cancel: %s\n" status;
         let%bind entries = Database.Database_exec.list_trade_log 5 in
         printf "audit log (newest first):\n";
         List.iter entries ~f:(fun entry ->
           printf !"  %{sexp: Trade_log_entry.t}\n" entry);
         return ())
;;

(* One section of the comparison report: header with a count, then entries
   best score first. Every row prints its deciding field, so review never has
   to guess why a pair sits where it does; rejected rows also name the veto
   the text gate read. *)
let print_section title entries =
  printf "\n== %s (%d) ==\n" title (List.length entries);
  List.sort
    entries
    ~compare:
      (Comparable.reverse
         (Comparable.lift Float.compare ~f:(fun (c : Matcher.Comparison.t) ->
            c.score)))
  |> List.iter
       ~f:
         (fun
           { Matcher.Comparison.candidate = { left; right }
           ; bucket
           ; score
           ; veto
           ; deciding
           ; _
           }
         ->
         let relation_note =
           match bucket with
           | Conflict relation ->
             [%string
               " %{Sexp.to_string [%sexp (relation : Claim.Relation.t)]}"]
           | Both | Claims_only | Text_only | Neither -> ""
         in
         let veto_note =
           match bucket, veto with
           | (Claims_only | Neither), Some reason ->
             [%string " [veto: %{reason}]"]
           | (Claims_only | Neither), None
           | (Both | Text_only | Conflict (_ : Claim.Relation.t)), _ ->
             ""
         in
         printf
           "  %.2f%s [%s]%s  %s  <->  %s\n"
           score
           relation_note
           deciding
           veto_note
           left.title
           right.title)
;;

(* Coverage table straight from the matcher's per-domain accounting: how many
   pairs the claims decided vs abstained on, and which field caused each
   abstention. *)
let print_coverage (coverage : Matcher.Comparison.Coverage.t) =
  printf "\n== claims coverage by domain ==\n";
  Map.iteri
    coverage
    ~f:
      (fun
        ~key:domain
        ~data:{ Matcher.Comparison.Coverage.Row.pairs; decided; abstentions }
      ->
      let abstained =
        Map.fold abstentions ~init:0 ~f:(fun ~key:_ ~data:count total ->
          total + count)
      in
      let causes =
        Map.to_alist abstentions
        |> List.map ~f:(fun (cause, count) ->
          [%string "%{count#Int}x %{cause}"])
        |> String.concat ~sep:", "
      in
      printf
        "  %-14s pairs %-8d decided %-8d abstained %-6d %s\n"
        domain
        pairs
        decided
        abstained
        causes)
;;

(* The golden set: labeled pairs frozen to a file, so "did the next run
   improve" is a diff, not a judgment call over a wall of text. *)
module Golden = struct
  module Entry = struct
    type t =
      { left : string (* market id *)
      ; right : string
      ; verdict : string
      ; left_title : string
      ; right_title : string
      }
    [@@deriving sexp]
  end

  type t = Entry.t list [@@deriving sexp]

  let verdict_string (bucket : Matcher.Comparison.Bucket.t) =
    Sexp.to_string [%sexp (bucket : Matcher.Comparison.Bucket.t)]
  ;;

  let entry (comparison : Matcher.Comparison.t) =
    let { Matcher.Candidate.left; right } = comparison.candidate in
    { Entry.left = Market_id.to_string left.market_id
    ; right = Market_id.to_string right.market_id
    ; verdict = verdict_string comparison.bucket
    ; left_title = left.title
    ; right_title = right.title
    }
  ;;

  (* Neither pairs are millions of junk collisions; the golden set holds the
     pairs a human can hold labels for. *)
  let worth_freezing (comparison : Matcher.Comparison.t) =
    match comparison.bucket with
    | Both | Claims_only | Text_only | Conflict (_ : Claim.Relation.t) ->
      true
    | Neither -> false
  ;;

  let write ~file comparisons =
    let golden =
      List.filter comparisons ~f:worth_freezing |> List.map ~f:entry
    in
    let%map.Deferred () =
      Writer.save file ~contents:(Sexp.to_string_hum [%sexp (golden : t)])
    in
    printf "\ngolden: froze %d pairs to %s\n" (List.length golden) file
  ;;

  (* Regressions (verdict changed) and disappearances (pair gone) print
     separately; pairs new since the freeze are already visible in the report
     buckets above. *)
  let diff ~file comparisons =
    let%map.Deferred contents = Reader.file_contents file in
    let golden = t_of_sexp (Sexp.of_string contents) in
    let current =
      List.map comparisons ~f:(fun comparison ->
        let entry = entry comparison in
        [%string "%{entry.left}|%{entry.right}"], entry.verdict)
      |> String.Map.of_alist_reduce ~f:(fun first (_ : string) -> first)
    in
    let changed, missing =
      List.partition_map golden ~f:(fun expected ->
        match
          Map.find current [%string "%{expected.left}|%{expected.right}"]
        with
        | None -> Second expected
        | Some verdict -> First (expected, verdict))
    in
    let changed =
      List.filter changed ~f:(fun (expected, verdict) ->
        not (String.equal expected.verdict verdict))
    in
    printf
      "\n== golden diff vs %s (%d frozen) ==\n"
      file
      (List.length golden);
    printf "regressions (verdict changed): %d\n" (List.length changed);
    List.iter changed ~f:(fun (expected, verdict) ->
      printf
        "  expected %s, got %s  %s  <->  %s\n"
        expected.verdict
        verdict
        expected.left_title
        expected.right_title);
    printf "missing (pair not judged this run): %d\n" (List.length missing);
    List.iter (List.take missing 10) ~f:(fun expected ->
      printf
        "  was %s  %s  <->  %s\n"
        expected.verdict
        expected.left_title
        expected.right_title)
  ;;
end

let compare_command =
  Command.async_or_error
    ~summary:
      "run the claim and text matchers side by side on live markets and \
       report where they agree and disagree (no LLM, no database writes)"
    (let%map_open.Command markets =
       flag
         "-markets"
         (optional_with_default 25 int)
         ~doc:
           "N highest-volume Kalshi markets to search from, exactly as \
            sweep does (default 25)"
     and full =
       flag
         "-full"
         no_arg
         ~doc:
           " page through every open market on both venues and compare the \
            full cross product instead of searching (slow)"
     and threshold =
       flag
         "-threshold"
         (optional_with_default 0.5 float)
         ~doc:"X text-pipeline similarity cutoff (default 0.5)"
     and golden_file =
       flag
         "-golden"
         (optional_with_default
            "lib/arbitrage/test/golden_pairs.expected"
            string)
         ~doc:
           "FILE labeled pairs to diff against (default \
            lib/arbitrage/test/golden_pairs.expected)"
     and write_golden =
       flag
         "-write-golden"
         no_arg
         ~doc:
           " freeze this run's non-Neither pairs to the golden file (edit \
            the labels afterwards; they are this run's verdicts)"
     and snapshot =
       flag
         "-snapshot"
         (optional string)
         ~doc:
           "FILE with -full: read the fetched markets from FILE if it \
            exists, else fetch and save them there — so matching can rerun \
            in seconds while tuning parsers, without refetching"
     in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let stopwatch = ref (Time_ns.now ()) in
       let lap label =
         let now = Time_ns.now () in
         Core.eprintf
           !"timing: %s took %{Time_ns.Span}\n"
           label
           (Time_ns.diff now !stopwatch);
         (* The matching phase blocks the scheduler for minutes; flush so
            timings survive even a killed run. *)
         Stdlib.flush Stdlib.stderr;
         stopwatch := now
       in
       let%bind report =
         if not full
         then
           Sweep.compare_once
             ~threshold
             ~apply_veto:true
             ~markets_to_sweep:markets
             ()
         else (
           let fetch venue =
             let%map markets =
               Market_data.Market_data_gateway.fetch_all_l1_market_data
                 ~venue
             in
             let stubs =
               List.filter markets ~f:L1_market_metadata.active
               |> List.map ~f:L1_market_metadata.to_market_stub
             in
             printf
               !"fetched %{Venue}: %d active markets\n"
               venue
               (List.length stubs);
             stubs
           in
           let%bind kalshi, poly =
             let fetch_both () =
               let%bind kalshi = fetch Kalshi in
               let%bind poly = fetch Polymarket in
               return (kalshi, poly)
             in
             match snapshot with
             | None -> fetch_both ()
             | Some file ->
               (match%bind.Deferred Sys.file_exists file with
                | `Yes ->
                  let%bind.Deferred contents = Reader.file_contents file in
                  let kalshi, poly =
                    [%of_sexp: Market_stub.t list * Market_stub.t list]
                      (Sexp.of_string contents)
                  in
                  printf
                    "snapshot: %d Kalshi / %d Polymarket markets from %s\n"
                    (List.length kalshi)
                    (List.length poly)
                    file;
                  return (kalshi, poly)
                | `No | `Unknown ->
                  let%bind markets = fetch_both () in
                  let%bind.Deferred () =
                    Writer.save
                      file
                      ~contents:
                        (Sexp.to_string
                           [%sexp
                             (markets
                              : Market_stub.t list * Market_stub.t list)])
                  in
                  printf "snapshot: saved to %s\n" file;
                  return markets)
           in
           lap "fetch/load";
           let comparisons =
             Matcher.compare_pipelines
               ~threshold
               ~apply_veto:true
               kalshi
               poly
           in
           lap "match";
           return comparisons)
       in
       let both, claims_only, text_only, conflicts =
         List.fold
           report.Matcher.Comparison.Report.judged
           ~init:([], [], [], [])
           ~f:(fun (both, claims, text, conflicts) comparison ->
             match comparison.Matcher.Comparison.bucket with
             | Both -> comparison :: both, claims, text, conflicts
             | Claims_only -> both, comparison :: claims, text, conflicts
             | Text_only -> both, claims, comparison :: text, conflicts
             | Conflict (_ : Claim.Relation.t) ->
               both, claims, text, comparison :: conflicts
             (* judged never holds Neither *)
             | Neither -> both, claims, text, conflicts)
       in
       print_section "both systems agree" both;
       print_section "claims only — text pipeline missed these" claims_only;
       print_section "text only — claims abstained (Opaque)" text_only;
       print_section "conflicts — text would propose, claims veto" conflicts;
       (* If a real twin hides among the near-misses, one of the gates
          (threshold, veto, claim parse) is too strict. *)
       print_section
         [%string
           "near misses — rejected by both systems, best %{List.length \
            report.near_misses#Int} of %{report.neither#Int}"]
         report.near_misses;
       printf
         "\n\
          summary: both %d / claims-only %d / text-only %d / conflict %d / \
          neither %d\n"
         (List.length both)
         (List.length claims_only)
         (List.length text_only)
         (List.length conflicts)
         report.neither;
       print_coverage report.coverage;
       if write_golden
       then Deferred.ok (Golden.write ~file:golden_file report.judged)
       else (
         match%bind.Deferred Sys.file_exists golden_file with
         | `Yes -> Deferred.ok (Golden.diff ~file:golden_file report.judged)
         | `No | `Unknown -> return ()))
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"cross-venue arbitrage pipeline"
       [ "sweep", sweep_command
       ; "review", review_command
       ; "run", run_command
       ; "check-trade", check_trade_command
       ; "compare", compare_command
       ])
;;
