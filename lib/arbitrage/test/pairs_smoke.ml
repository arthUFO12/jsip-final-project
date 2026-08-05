open! Core
open! Async
open! Types
open Arbitrage

(* Step-1 smoke: the pair store working on live markets, with one real LLM
   adjudication. Fetches both venues' listings, takes the single best-scoring
   cross-venue candidate, asks the LLM whether it's the same event, records
   the proposal in the real database, applies the verdict as the review
   decision (a stand-in for the human gate), and prints the store. Needs
   ANTHROPIC_API_KEY. Run with: set -a; source .env; set +a; dune exec
   lib/arbitrage/test/pairs_smoke.exe *)

let db_path = "/home/ubuntu/jsip-final-project-1/arbiter.db"
let scan_limit = 200

let insert_stub_if_missing (stub : Market_stub.t) =
  match%bind
    Database.Database_exec.find_market_stub stub.market_id >>| ok_exn
  with
  | Some (_ : Market_stub.t) -> Deferred.unit
  | None -> Database.Database_exec.insert_market_stub stub >>| ok_exn
;;

let print_store () =
  Deferred.List.iter
    ~how:`Sequential
    Pair_proposal.Status.[ Proposed; Approved; Rejected ]
    ~f:(fun status ->
      let%map pairs =
        Database.Database_exec.list_pair_proposals_by_status status
        >>| ok_exn
      in
      print_s
        [%message
          ""
            ~_:(status : Pair_proposal.Status.t)
            ~_:(pairs : Pair_proposal.t list)])
;;

let main () =
  let open Deferred.Or_error.Let_syntax in
  ok_exn (Database.Database_exec.init_database db_path);
  let%bind () = Database.Database_exec.create_market_stub_table () in
  let%bind () = Database.Database_exec.create_pair_proposal_table () in
  let%bind () = Database.Database_exec.create_pair_stub_table () in
  let%bind kalshi =
    Market_data.Market_data_gateway.fetch_l1_market_data
      ~venue:Kalshi
      ~closed:false
      ~limit:scan_limit
      ()
  in
  let%bind poly =
    Market_data.Market_data_gateway.fetch_l1_market_data
      ~venue:Polymarket
      ~closed:false
      ~limit:scan_limit
      ()
  in
  let stubs markets =
    List.map markets ~f:L1_market_metadata.to_market_stub
  in
  let best_candidate =
    Matcher.For_testing.block (stubs kalshi) (stubs poly)
    |> List.map ~f:(fun candidate ->
      Matcher.For_testing.score candidate, candidate)
    |> List.max_elt
         ~compare:
           (Comparable.lift Float.compare ~f:(fun (score, _) -> score))
  in
  match best_candidate with
  | None -> Deferred.Or_error.error_string "no cross-venue candidates at all"
  | Some (score, ({ left; right } as candidate)) ->
    printf "best live candidate (score %.2f):\n" score;
    print_endline [%string "  %{left.title}  <->  %{right.title}"];
    let%bind verdict = Llm_matcher.adjudicate candidate in
    print_s [%message "llm verdict" ~_:(verdict : Llm_matcher.verdict)];
    let%bind () = Deferred.ok (insert_stub_if_missing left) in
    let%bind () = Deferred.ok (insert_stub_if_missing right) in
    let proposal =
      Pair_proposal.create
        ~left:left.market_id
        ~right:right.market_id
        ~score
        ~explanation:(Some verdict.explanation)
    in
    let%bind () = Database.Database_exec.propose_pair proposal in
    (* Stand-in for the human reviewer: follow the LLM's advice. *)
    let decision : Pair_proposal.Status.t =
      if verdict.is_match then Approved else Rejected
    in
    let%bind () =
      Database.Database_exec.set_pair_status
        ~left:proposal.left
        ~right:proposal.right
        decision
    in
    Deferred.ok (print_store ())
;;

let () =
  don't_wait_for
    (let%bind.Deferred result = main () in
     (match result with
      | Ok () -> ()
      | Error error -> Core.eprint_s [%sexp (error : Error.t)]);
     shutdown 0;
     Deferred.unit);
  never_returns (Scheduler.go ())
;;
