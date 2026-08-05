open! Core
open! Async
open Types

(* Markets observed live on the venues (titles verbatim; the Polymarket ids
   stand in for gamma's numeric ids). The BTC pair is the shape a sweep
   should approve — same event on both venues; the Whitmer pair is the trap
   it should reject — same person, different event. *)
let stub ~venue ~id ~title ~category =
  { Market_stub.venue
  ; market_id = Market_id.of_string id
  ; slug = Slug.of_string id
  ; series_ticker = None
  ; clob_token_id = None
  ; title
  ; category
  ; created_time = Time_ns.epoch
  ; close_time = Time_ns.of_string "2030-01-01 00:00:00Z"
  ; volume = None
  }
;;

let kalshi_btc =
  stub
    ~venue:Kalshi
    ~id:"KXBTCD-25DEC31-T120000"
    ~category:Category.Crypto
    ~title:"Will Bitcoin be above $120,000 on Dec 31?"
;;

let poly_btc =
  stub
    ~venue:Polymarket
    ~id:"531417"
    ~category:Category.Crypto
    ~title:"Bitcoin above $120,000 on December 31?"
;;

let kalshi_whitmer =
  stub
    ~venue:Kalshi
    ~id:"KXDNCCHAIR-WHITMER"
    ~category:Category.Politics
    ~title:
      "Will Gretchen Whitmer be the next Chair of the Democratic National \
       Committee?"
;;

let poly_whitmer =
  stub
    ~venue:Polymarket
    ~id:"512294"
    ~category:Category.Politics
    ~title:
      "Will Gretchen Whitmer win the 2028 Democratic presidential \
       nomination?"
;;

let print_pairs status =
  let%map pairs =
    Database.Database_exec.list_pair_proposals_by_status status
  in
  print_s
    [%message
      ""
        ~_:(status : Pair_proposal.Status.t)
        ~_:(ok_exn pairs : Pair_proposal.t list)]
;;

let%expect_test "pair store lifecycle on real markets" =
  (* A fresh database file per run so the story below is deterministic. *)
  let db_path = "/tmp/arbiter-test-pairs.db" in
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
  let%bind () =
    Deferred.List.iter
      ~how:`Sequential
      [ kalshi_btc; poly_btc; kalshi_whitmer; poly_whitmer ]
      ~f:(fun stub ->
        Database.Database_exec.insert_market_stub stub >>| ok_exn)
  in
  (* Proposing normalizes side order: both spellings are the same pair. *)
  let btc_pair =
    Pair_proposal.create
      ~left:kalshi_btc.market_id
      ~right:poly_btc.market_id
      ~score:0.62
      ~explanation:None
  in
  let btc_pair_swapped =
    Pair_proposal.create
      ~left:poly_btc.market_id
      ~right:kalshi_btc.market_id
      ~score:0.62
      ~explanation:None
  in
  print_s [%sexp (btc_pair : Pair_proposal.t)];
  print_s [%sexp (btc_pair_swapped : Pair_proposal.t)];
  [%expect
    {|
    ((left 531417) (right KXBTCD-25DEC31-T120000) (score 0.62) (explanation ())
     (status Proposed))
    ((left 531417) (right KXBTCD-25DEC31-T120000) (score 0.62) (explanation ())
     (status Proposed))
    |}];
  let whitmer_pair =
    Pair_proposal.create
      ~left:kalshi_whitmer.market_id
      ~right:poly_whitmer.market_id
      ~score:0.37
      ~explanation:
        (Some
           "Different events: DNC Chair selection and the 2028 presidential \
            nomination resolve independently.")
  in
  let%bind () = Database.Database_exec.propose_pair btc_pair >>| ok_exn in
  let%bind () =
    Database.Database_exec.propose_pair btc_pair_swapped >>| ok_exn
  in
  let%bind () =
    Database.Database_exec.propose_pair whitmer_pair >>| ok_exn
  in
  (* The swapped re-proposal collapsed into one row; best score first. *)
  let%bind () = print_pairs Proposed in
  [%expect
    {|
    (Proposed
     (((left 531417) (right KXBTCD-25DEC31-T120000) (score 0.62) (explanation ())
       (status Proposed))
      ((left 512294) (right KXDNCCHAIR-WHITMER) (score 0.37)
       (explanation
        ("Different events: DNC Chair selection and the 2028 presidential nomination resolve independently."))
       (status Proposed))))
    |}];
  (* The human decides: BTC pair is the same event, Whitmer pair is not. *)
  let%bind () =
    Database.Database_exec.set_pair_status
      ~left:btc_pair.left
      ~right:btc_pair.right
      Approved
    >>| ok_exn
  in
  let%bind () =
    Database.Database_exec.set_pair_status
      ~left:whitmer_pair.left
      ~right:whitmer_pair.right
      Rejected
    >>| ok_exn
  in
  let%bind () = print_pairs Proposed in
  let%bind () = print_pairs Approved in
  let%bind () = print_pairs Rejected in
  [%expect
    {|
    (Proposed ())
    (Approved
     (((left 531417) (right KXBTCD-25DEC31-T120000) (score 0.62) (explanation ())
       (status Approved))))
    (Rejected
     (((left 512294) (right KXDNCCHAIR-WHITMER) (score 0.37)
       (explanation
        ("Different events: DNC Chair selection and the 2028 presidential nomination resolve independently."))
       (status Rejected))))
    |}];
  (* A later sweep re-proposes the rejected pair with a better score: the
     decision must hold and the row must not change. *)
  let%bind () =
    Database.Database_exec.propose_pair { whitmer_pair with score = 0.9 }
    >>| ok_exn
  in
  let%bind () = print_pairs Proposed in
  let%bind () = print_pairs Rejected in
  [%expect
    {|
    (Proposed ())
    (Rejected
     (((left 512294) (right KXDNCCHAIR-WHITMER) (score 0.37)
       (explanation
        ("Different events: DNC Chair selection and the 2028 presidential nomination resolve independently."))
       (status Rejected))))
    |}];
  (* The review view: the approved pair joined back to its stubs, which is
     what a human (or the bot) actually reads. *)
  let%bind approved =
    Database.Database_exec.list_pair_proposals_by_status Approved >>| ok_exn
  in
  Deferred.List.iter
    ~how:`Sequential
    approved
    ~f:(fun { left; right; score = _; explanation = _; status = _ } ->
      let title market_id =
        match%map
          Database.Database_exec.find_market_stub market_id >>| ok_exn
        with
        | Some (stub : Market_stub.t) -> stub.title
        | None -> "<no stub>"
      in
      let%bind left_title = title left in
      let%map right_title = title right in
      print_endline [%string "%{left_title}  <->  %{right_title}"])
  >>| fun () ->
  [%expect
    {| Bitcoin above $120,000 on December 31?  <->  Will Bitcoin be above $120,000 on Dec 31? |}]
;;

let%expect_test "pair legs survive the catalog purge via their snapshots" =
  let db_path = "/tmp/arbiter-test-pair-stubs.db" in
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
  let%bind () =
    Database.Database_exec.create_pair_stub_table () >>| ok_exn
  in
  (* An old store: the pair was proposed before snapshots existed, so its
     legs live only in the rotating catalog. *)
  let%bind () =
    Deferred.List.iter
      ~how:`Sequential
      [ kalshi_btc; poly_btc ]
      ~f:(fun stub ->
        Database.Database_exec.insert_market_stub stub >>| ok_exn)
  in
  let%bind () =
    Database.Database_exec.propose_pair
      (Pair_proposal.create
         ~left:kalshi_btc.market_id
         ~right:poly_btc.market_id
         ~score:0.62
         ~explanation:None)
    >>| ok_exn
  in
  let print_legs () =
    let title market_id =
      match%map
        Database.Database_exec.find_pair_stub market_id >>| ok_exn
      with
      | Some (stub : Market_stub.t) -> stub.title
      | None -> "<no stub>"
    in
    let%bind left_title = title kalshi_btc.market_id in
    let%map right_title = title poly_btc.market_id in
    print_endline [%string "%{left_title}  <->  %{right_title}"]
  in
  (* Before any rescue, the lookup falls back to the catalog. *)
  let%bind () = print_legs () in
  [%expect
    {| Will Bitcoin be above $120,000 on Dec 31?  <->  Bitcoin above $120,000 on December 31? |}];
  (* Startup order: backfill snapshots, then the seed purges the catalog. The
     pair keeps its legs. *)
  let%bind () = Database.Database_exec.backfill_pair_stubs () >>| ok_exn in
  let%bind () =
    Database.Database_exec.delete_market_stubs_by_ids
      [ kalshi_btc.market_id; poly_btc.market_id ]
    >>| ok_exn
  in
  let%bind () = print_legs () in
  [%expect
    {| Will Bitcoin be above $120,000 on Dec 31?  <->  Bitcoin above $120,000 on December 31? |}];
  return ()
;;
