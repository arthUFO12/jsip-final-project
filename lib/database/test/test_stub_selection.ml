(* Tests for {!Database.Stub_selection}: the pure purge/replace policy the
   rotating seed uses. *)

open! Core
open Types
open Database

let stub ?volume id : Market_stub.t =
  { venue = Venue.of_string "Kalshi"
  ; market_id = Market_id.of_string id
  ; slug = Slug.of_string id
  ; series_ticker = None
  ; clob_token_id = None
  ; title = id
  ; created_time = Time_ns.of_string "2026-01-01 00:00:00Z"
  ; close_time = Time_ns.of_string "2026-12-01 00:00:00Z"
  ; category = Category.Miscellaneous
  ; volume = Option.map volume ~f:(fun v -> Volume.Contracts (Size.of_int v))
  }
;;

let ids stubs =
  List.map stubs ~f:(fun (stub : Market_stub.t) -> stub.market_id)
;;

let%expect_test "live purge victims: lowest volume first, missing volume \
                 lowest of all"
  =
  let stubs =
    [ stub "busy" ~volume:9_000
    ; stub "quiet" ~volume:3
    ; stub "unknown"
    ; stub "middling" ~volume:400
    ]
  in
  print_s
    [%sexp
      (Stub_selection.live_purge_victims stubs ~count:2 : Market_id.t list)];
  [%expect {| (unknown quiet) |}]
;;

let%expect_test "select_new prefers markets never seen before" =
  let fetched =
    [ stub "already-stored"; stub "just-purged"; stub "brand-new" ]
  in
  let chosen, readds =
    Stub_selection.select_new
      ~fetched
      ~existing_ids:
        (Market_id.Set.of_list [ Market_id.of_string "already-stored" ])
      ~purged_ids:
        (Market_id.Set.of_list [ Market_id.of_string "just-purged" ])
      ~needed:1
  in
  print_s
    [%message "" ~chosen:(ids chosen : Market_id.t list) (readds : int)];
  [%expect {| ((chosen (brand-new)) (readds 0)) |}]
;;

let%expect_test "select_new re-adds purged markets only when the listing \
                 runs dry, and says how many"
  =
  let fetched =
    [ stub "already-stored"; stub "just-purged"; stub "brand-new" ]
  in
  let chosen, readds =
    Stub_selection.select_new
      ~fetched
      ~existing_ids:
        (Market_id.Set.of_list [ Market_id.of_string "already-stored" ])
      ~purged_ids:
        (Market_id.Set.of_list [ Market_id.of_string "just-purged" ])
      ~needed:3
  in
  print_s
    [%message "" ~chosen:(ids chosen : Market_id.t list) (readds : int)];
  [%expect {| ((chosen (brand-new just-purged)) (readds 1)) |}]
;;

let%expect_test "select_new never exceeds needed and tolerates an empty \
                 fetch"
  =
  let chosen, readds =
    Stub_selection.select_new
      ~fetched:[]
      ~existing_ids:Market_id.Set.empty
      ~purged_ids:Market_id.Set.empty
      ~needed:5
  in
  print_s
    [%message "" ~chosen:(ids chosen : Market_id.t list) (readds : int)];
  [%expect {| ((chosen ()) (readds 0)) |}]
;;
