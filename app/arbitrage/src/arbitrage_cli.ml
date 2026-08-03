open! Core
open! Async
open! Types
open Arbitrage

(* The command-line face of the arbitrage pipeline. Four verbs: [sweep] files
   pair proposals with the review gate, [review] lets a human approve or
   reject them, [run] prices the approved set on a timer — those three share
   one sqlite store — and [compare] prints the claim and text matchers'
   verdicts side by side without touching the store. *)

let init_store ~db =
  let open Deferred.Or_error.Let_syntax in
  let%bind cwd = Deferred.ok (Sys.getcwd ()) in
  let db = if Filename.is_absolute db then db else cwd ^/ db in
  let%bind () = Deferred.return (Database.Database_exec.init_database db) in
  let%bind () = Database.Database_exec.create_market_stub_table () in
  Database.Database_exec.create_pair_proposal_table ()
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
         let%bind decided = Review.decide ~index ~status in
         printf !"%{sexp: Pair_proposal.Status.t}: " status;
         print_endline (Review.Listed.to_display_string decided);
         return ()
       in
       let list (status : Pair_proposal.Status.t) =
         let%bind listed = Review.list_by_status status in
         printf
           !"%d %{sexp: Pair_proposal.Status.t} pair(s):\n"
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
    ~summary:"price the approved pairs on a timer and report edges (paper)"
    (let%map_open.Command db = db_flag
     and reckless =
       flag
         "-reckless"
         no_arg
         ~doc:
           " ignore fees and depth when pricing (never tradable; see docs)"
     in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind () = init_store ~db in
       let config =
         { Config.default with
           execution =
             { Config.Execution.default with
               detect_mode = (if reckless then Reckless else Exact)
             }
         }
       in
       Bot.run ~config)
;;

(* One section of the comparison report: header with a count, then entries
   best score first. Every row prints its deciding field, so review never
   has to guess why a pair sits where it does; rejected rows also name
   the veto the text gate read. *)
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
           | (Both | Text_only | Conflict (_ : Claim.Relation.t)), _ -> ""
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

(* Which domain a pair belongs to, for the coverage table. *)
let comparison_domain (comparison : Matcher.Comparison.t) =
  match comparison.left_claim, comparison.right_claim with
  | Some claim, (Some _ | None) | None, Some claim ->
    Claim.Domain.to_string claim.domain
  | None, None -> "(unparsed)"
;;

(* Coverage table: for each domain, how many pairs the claims decided vs
   abstained on, and which field caused each abstention — so "claims only
   speak rates" is a table row, not an inference from reading 260 titles. *)
let print_coverage comparisons =
  printf "\n== claims coverage by domain ==\n";
  List.sort_and_group
    comparisons
    ~compare:(Comparable.lift String.compare ~f:comparison_domain)
  |> List.iter ~f:(fun group ->
    let domain = comparison_domain (List.hd_exn group) in
    let abstained, decided =
      List.partition_tf group ~f:(fun (c : Matcher.Comparison.t) ->
        String.is_prefix c.deciding ~prefix:"abstained")
    in
    let decided =
      List.filter decided ~f:(fun (c : Matcher.Comparison.t) ->
        not (String.equal c.deciding "unparsed"))
    in
    let abstention_causes =
      List.sort_and_group
        abstained
        ~compare:
          (Comparable.lift
             String.compare
             ~f:(fun (c : Matcher.Comparison.t) -> c.deciding))
      |> List.map ~f:(fun cause ->
        [%string
          "%{List.length cause#Int}x %{(List.hd_exn cause).deciding}"])
      |> String.concat ~sep:", "
    in
    printf
      "  %-14s pairs %-8d decided %-8d abstained %-6d %s\n"
      domain
      (List.length group)
      (List.length decided)
      (List.length abstained)
      abstention_causes)
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

  (* Neither pairs are millions of junk collisions; the golden set holds
     the pairs a human can hold labels for. *)
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
     separately; pairs new since the freeze are already visible in the
     report buckets above. *)
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
     in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind comparisons =
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
           let%bind kalshi = fetch Kalshi in
           let%bind poly = fetch Polymarket in
           return
             (Matcher.compare_pipelines
                ~threshold
                ~apply_veto:true
                kalshi
                poly))
       in
       let both, claims_only, text_only, conflicts, neither =
         List.fold
           comparisons
           ~init:([], [], [], [], [])
           ~f:(fun (both, claims, text, conflicts, neither) comparison ->
             match comparison.Matcher.Comparison.bucket with
             | Both -> comparison :: both, claims, text, conflicts, neither
             | Claims_only ->
               both, comparison :: claims, text, conflicts, neither
             | Text_only ->
               both, claims, comparison :: text, conflicts, neither
             | Conflict (_ : Claim.Relation.t) ->
               both, claims, text, comparison :: conflicts, neither
             | Neither ->
               both, claims, text, conflicts, comparison :: neither)
       in
       print_section "both systems agree" both;
       print_section "claims only — text pipeline missed these" claims_only;
       print_section "text only — claims abstained (Opaque)" text_only;
       print_section "conflicts — text would propose, claims veto" conflicts;
       (* The near-misses can number in the thousands under -full; only the
          best-scoring few are worth eyes. If a real twin hides here, one of
          the gates (threshold, veto, claim parse) is too strict. *)
       let near_miss_display_limit = 10 in
       print_section
         [%string
           "near misses — rejected by both systems, best \
            %{near_miss_display_limit#Int} of %{List.length neither#Int}"]
         (List.take
            (List.sort
               neither
               ~compare:
                 (Comparable.reverse
                    (Comparable.lift
                       Float.compare
                       ~f:(fun (c : Matcher.Comparison.t) -> c.score))))
            near_miss_display_limit);
       printf
         "\n\
          summary: both %d / claims-only %d / text-only %d / conflict %d / \
          neither %d\n"
         (List.length both)
         (List.length claims_only)
         (List.length text_only)
         (List.length conflicts)
         (List.length neither);
       print_coverage comparisons;
       if write_golden
       then Deferred.ok (Golden.write ~file:golden_file comparisons)
       else (
         match%bind.Deferred Sys.file_exists golden_file with
         | `Yes ->
           Deferred.ok (Golden.diff ~file:golden_file comparisons)
         | `No | `Unknown -> return ()))
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"cross-venue arbitrage pipeline"
       [ "sweep", sweep_command
       ; "review", review_command
       ; "run", run_command
       ; "compare", compare_command
       ])
;;
