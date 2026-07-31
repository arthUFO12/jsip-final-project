open! Core
open! Async
open! Types
open Arbitrage

(* The command-line face of the arbitrage pipeline. Three verbs: [sweep]
   files pair proposals with the review gate, [review] lets a human approve
   or reject them, [run] prices the approved set on a timer. All three share
   one sqlite store. *)

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
     in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind () = init_store ~db in
       let matching =
         if use_llm
         then Config.Matching.default_llm_assisted
         else Config.Matching.default_text_only
       in
       let%bind summary =
         if full
         then Sweep.sweep_full ~matching ~max_adjudications:max_llm ()
         else Sweep.sweep_once ~matching ~markets_to_sweep:markets ()
       in
       print_s [%sexp (summary : Sweep.summary)];
       return ())
;;

let print_proposal
  index
  ({ left; right; score; explanation; status = _ } : Pair_proposal.t)
  =
  let open Deferred.Or_error.Let_syntax in
  let title market_id =
    match%map Database.Database_exec.find_market_stub market_id with
    | Some (stub : Market_stub.t) ->
      [%string "%{stub.title} [%{stub.venue#Venue}]"]
    | None -> [%string "<no stub for %{market_id#Market_id}>"]
  in
  let%bind left_title = title left in
  let%bind right_title = title right in
  printf "%2d. [%.2f] %s  <->  %s\n" index score left_title right_title;
  (match explanation with
   | None -> ()
   | Some explanation -> printf "      llm: %s\n" explanation);
  return ()
;;

let review_command =
  Command.async_or_error
    ~summary:
      "list proposed pairs, or approve/reject one by its listed number"
    (let%map_open.Command db = db_flag
     and approve =
       flag "-approve" (optional int) ~doc:"N approve the Nth listed pair"
     and reject =
       flag "-reject" (optional int) ~doc:"N reject the Nth listed pair"
     in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind () = init_store ~db in
       let%bind proposed =
         Database.Database_exec.list_pair_proposals_by_status Proposed
       in
       let decide index (status : Pair_proposal.Status.t) =
         match List.nth proposed index with
         | None ->
           Deferred.Or_error.error_s
             [%message
               "no such proposal"
                 (index : int)
                 ~listed:(List.length proposed : int)]
         | Some pair ->
           let%bind () =
             Database.Database_exec.set_pair_status
               ~left:pair.left
               ~right:pair.right
               status
           in
           printf !"%{sexp: Pair_proposal.Status.t}: " status;
           print_proposal index pair
       in
       match approve, reject with
       | Some _, Some _ ->
         Deferred.Or_error.error_string "pass -approve or -reject, not both"
       | Some index, None -> decide index Approved
       | None, Some index -> decide index Rejected
       | None, None ->
         printf
           "%d proposed pair(s) awaiting review:\n"
           (List.length proposed);
         Deferred.Or_error.List.iteri
           ~how:`Sequential
           proposed
           ~f:(fun index pair -> print_proposal index pair))
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

let () =
  Command_unix.run
    (Command.group
       ~summary:"cross-venue arbitrage pipeline"
       [ "sweep", sweep_command
       ; "review", review_command
       ; "run", run_command
       ])
;;
