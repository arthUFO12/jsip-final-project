open! Core
open Async
open Types
open Database

let real_id = Market_id.of_string "12345"
let fake_id = Market_id.of_string "23456"
let db_name = "test.db"
let db_dir = "/tmp/"

let file_exists file_name =
  let%bind db_existence = Sys.file_exists file_name in
  match db_existence with `Yes -> return true | _ -> return false
;;

let make_stub
  ?(category = Category.Miscellaneous)
  ?volume
  ~venue
  ~market_id
  ~slug
  ~title
  ~created_time
  ~close_time
  ()
  : Market_stub.t
  =
  { venue = Venue.of_string venue
  ; market_id
  ; slug = Slug.of_string slug
  ; series_ticker = None
  ; clob_token_id = None
  ; title
  ; category = Miscellaneous
  ; created_time = Time_ns.of_string created_time
  ; close_time = Time_ns.of_string close_time
  ; category
  ; volume
  }
;;

let%expect_test "successfully connect to database" =
  let%bind db_exists = file_exists (db_dir ^ db_name) in
  let%bind () =
    if db_exists then Sys.remove (db_dir ^ db_name) else return ()
  in
  (match Database_exec.init_database (db_dir ^ db_name) with
   | Ok () -> print_endline "Database connection is online!"
   | Error e -> print_endline (Error.to_string_hum e));
  return [%expect {| Database connection is online! |}]
;;

let%expect_test "market stub table is successfully created" =
  let%bind success_or_error = Database_exec.create_market_stub_table () in
  let%bind () =
    match success_or_error with
    | Ok () ->
      let%bind db_exists = file_exists (db_dir ^ db_name) in
      if db_exists
      then (
        print_endline "Database is created";
        return ())
      else (
        print_endline "Database was not created or existence unknown";
        return ())
    | Error e ->
      print_endline (Error.to_string_hum e);
      return ()
  in
  return [%expect {| Database is created |}]
;;

let%expect_test "market stub is successfully inserted into the table" =
  let stub =
    make_stub
      ~venue:"Polymarket"
      ~market_id:real_id
      ~slug:"12345"
      ~title:"Don trump tweets"
      ~created_time:"2026-07-01T00:00:00Z"
      ~close_time:"2026-07-25T15:45:00Z"
      ()
  in
  let%bind success_or_error = Database_exec.insert_market_stub stub in
  (match success_or_error with
   | Ok () -> print_endline "row successfully created"
   | Error e -> print_endline (Error.to_string_hum e));
  return [%expect {| row successfully created |}]
;;

let%expect_test "re-inserting an existing id replaces the row (upsert)" =
  let stub =
    make_stub
      ~venue:"Kalshi"
      ~market_id:real_id
      ~slug:"12345"
      ~title:"France wins world cup"
      ~created_time:"2026-07-01T00:00:00Z"
      ~close_time:"2026-08-15T00:00:00Z"
      ()
  in
  let%bind success_or_error = Database_exec.insert_market_stub stub in
  (match success_or_error with
   | Ok () -> print_endline "row successfully replaced"
   | Error e -> print_endline (Error.to_string_hum e));
  return [%expect {| row successfully replaced |}]
;;

let%expect_test "market stub is successfully found" =
  let%bind stub_option_or_error = Database_exec.find_market_stub real_id in
  (match stub_option_or_error with
   | Ok stub_option ->
     (match stub_option with
      | None -> print_endline "market id not found"
      | Some stub ->
        print_endline [%string "Stub found: %{stub#Market_stub}"])
   | Error e -> print_endline (Error.to_string_hum e));
  return
    [%expect
      {| Stub found: venue: Kalshi, market_id: 12345, slug: 12345, title: France wins world cup, created_time: 2026-07-01 00:00:00.000000000Z, close_time: 2026-08-15 00:00:00.000000000Z |}]
;;

let%expect_test "Nonexistent market ID is not found" =
  let%bind stub_option_or_error = Database_exec.find_market_stub fake_id in
  (match stub_option_or_error with
   | Ok stub_option ->
     (match stub_option with
      | None -> print_endline "market id not found"
      | Some stub ->
        print_endline [%string "Stub found: %{stub#Market_stub}"])
   | Error e -> print_endline (Error.to_string_hum e));
  return [%expect {| market id not found |}]
;;

let%expect_test "Market stubs after 2026-07-19 are found" =
  let%bind stub_list_or_error =
    Database_exec.For_testing.list_current_market_stubs
      5
      ~time:(Time_ns.of_string "2026-07-19T00:00:00Z")
  in
  (match stub_list_or_error with
   | Ok stub_list ->
     List.iter stub_list ~f:(fun stub ->
       print_endline (Market_stub.to_string stub))
   | Error e -> print_endline (Error.to_string_hum e));
  return
    [%expect
      {| venue: Kalshi, market_id: 12345, slug: 12345, title: France wins world cup, created_time: 2026-07-01 00:00:00.000000000Z, close_time: 2026-08-15 00:00:00.000000000Z |}]
;;

let historical_id = Market_id.of_string "34567"

let%expect_test "category and volume survive a round-trip" =
  let stub =
    make_stub
      ~category:Category.Sports
      ~volume:(Volume.Contracts (Size.of_int 4200))
      ~venue:"Kalshi"
      ~market_id:historical_id
      ~slug:"old-final"
      ~title:"Old cup final"
      ~created_time:"2026-06-01T00:00:00Z"
      ~close_time:"2026-06-30T00:00:00Z"
      ()
  in
  let%bind success_or_error = Database_exec.insert_market_stubs [ stub ] in
  (match success_or_error with
   | Ok () -> print_endline "row successfully created"
   | Error e -> print_endline (Error.to_string_hum e));
  [%expect {| row successfully created |}];
  let%bind found_or_error = Database_exec.find_market_stub historical_id in
  (match found_or_error with
   | Ok None -> print_endline "market id not found"
   | Ok (Some { category; volume; _ }) ->
     print_s [%message "" (category : Category.t) (volume : Volume.t option)]
   | Error e -> print_endline (Error.to_string_hum e));
  return [%expect {| ((category Sports) (volume ((Contracts 4200)))) |}]
;;

let%expect_test "stubs are also findable by slug" =
  let%bind found_or_error =
    Database_exec.find_market_stub_by_slug (Slug.of_string "old-final")
  in
  (match found_or_error with
   | Ok None -> print_endline "slug not found"
   | Ok (Some stub) -> print_endline [%string "found: %{stub#Market_stub}"]
   | Error e -> print_endline (Error.to_string_hum e));
  [%expect
    {| found: venue: Kalshi, market_id: 34567, slug: old-final, title: Old cup final, created_time: 2026-06-01 00:00:00.000000000Z, close_time: 2026-06-30 00:00:00.000000000Z |}];
  let%bind missing_or_error =
    Database_exec.find_market_stub_by_slug (Slug.of_string "no-such")
  in
  (match missing_or_error with
   | Ok None -> print_endline "slug not found"
   | Ok (Some stub) -> print_endline [%string "found: %{stub#Market_stub}"]
   | Error e -> print_endline (Error.to_string_hum e));
  return [%expect {| slug not found |}]
;;

let%expect_test "Market stubs closed before 2026-07-19 are historical" =
  let%bind stub_list_or_error =
    Database_exec.For_testing.list_historical_market_stubs
      5
      ~time:(Time_ns.of_string "2026-07-19T00:00:00Z")
  in
  (match stub_list_or_error with
   | Ok stub_list ->
     List.iter stub_list ~f:(fun stub ->
       print_endline (Market_stub.to_string stub))
   | Error e -> print_endline (Error.to_string_hum e));
  return
    [%expect
      {| venue: Kalshi, market_id: 34567, slug: old-final, title: Old cup final, created_time: 2026-06-01 00:00:00.000000000Z, close_time: 2026-06-30 00:00:00.000000000Z |}]
;;

let%expect_test "counts split current from historical at the injected time" =
  let time = Time_ns.of_string "2026-07-19T00:00:00Z" in
  let%bind current =
    Database_exec.For_testing.count_current_market_stubs ~time
  in
  let%bind historical =
    Database_exec.For_testing.count_historical_market_stubs ~time
  in
  print_s
    [%message "" (current : int Or_error.t) (historical : int Or_error.t)];
  return [%expect {| ((current (Ok 1)) (historical (Ok 1))) |}]
;;

let ancient_id = Market_id.of_string "45678"

let%expect_test "oldest historical stubs come back oldest close time first" =
  let stub =
    make_stub
      ~venue:"Kalshi"
      ~market_id:ancient_id
      ~slug:"ancient-final"
      ~title:"Ancient cup final"
      ~created_time:"2026-01-01T00:00:00Z"
      ~close_time:"2026-01-31T00:00:00Z"
      ()
  in
  let%bind (_ : unit Or_error.t) =
    Database_exec.insert_market_stubs [ stub ]
  in
  let%bind stub_list_or_error =
    Database_exec.For_testing.list_oldest_historical_market_stubs
      5
      ~time:(Time_ns.of_string "2026-07-19T00:00:00Z")
  in
  (match stub_list_or_error with
   | Ok stub_list ->
     List.iter stub_list ~f:(fun (stub : Market_stub.t) ->
       print_endline [%string "%{stub.slug#Slug}"])
   | Error e -> print_endline (Error.to_string_hum e));
  return [%expect {|
    ancient-final
    old-final
    |}]
;;

let%expect_test "deleting by id removes exactly the named rows" =
  let%bind success_or_error =
    Database_exec.delete_market_stubs_by_ids [ historical_id; ancient_id ]
  in
  (match success_or_error with
   | Ok () -> print_endline "rows deleted"
   | Error e -> print_endline (Error.to_string_hum e));
  [%expect {| rows deleted |}];
  let time = Time_ns.of_string "2026-07-19T00:00:00Z" in
  let%bind current =
    Database_exec.For_testing.count_current_market_stubs ~time
  in
  let%bind historical =
    Database_exec.For_testing.count_historical_market_stubs ~time
  in
  print_s
    [%message "" (current : int Or_error.t) (historical : int Or_error.t)];
  return [%expect {| ((current (Ok 1)) (historical (Ok 0))) |}]
;;
