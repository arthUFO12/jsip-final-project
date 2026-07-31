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

let make_stub ~venue ~market_id ~slug ~title ~close_time : Market_stub.t =
  { venue = Venue.of_string venue
  ; market_id
  ; slug = Slug.of_string slug
  ; series_ticker = None
  ; clob_token_id = None
  ; title
  ; category = Miscellaneous
  ; close_time = Some (Time_ns.of_string close_time)
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
      ~close_time:"2026-07-25T15:45:00Z"
  in
  let%bind success_or_error = Database_exec.insert_market_stub stub in
  (match success_or_error with
   | Ok () -> print_endline "row successfully created"
   | Error e -> print_endline (Error.to_string_hum e));
  return [%expect {| row successfully created |}]
;;

let%expect_test "duplicate stub id is not inserted into the table" =
  let stub =
    make_stub
      ~venue:"Kalshi"
      ~market_id:real_id
      ~slug:"12345"
      ~title:"France wins world cup"
      ~close_time:"2026-08-15T00:00:00Z"
  in
  let%bind success_or_error = Database_exec.insert_market_stub stub in
  (match success_or_error with
   | Ok () -> print_endline "row successfully created"
   | Error e -> print_endline (Error.to_string_hum e));
  return
    [%expect
      {| Response from <sqlite3:///tmp/test.db> failed: UNIQUE constraint failed: market_stubs.market_id. Query: " INSERT INTO market_stubs (venue, market_id, slug, series_ticker, clob_token_id, title, category, close_time) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8) ". |}]
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
      {| Stub found: venue: Polymarket, market_id: 12345, slug: 12345, title: Don trump tweets, category: Miscellaneous, close_time: 2026-07-25 15:45:00.000000000Z |}]
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
      {| venue: Polymarket, market_id: 12345, slug: 12345, title: Don trump tweets, category: Miscellaneous, close_time: 2026-07-25 15:45:00.000000000Z |}]
;;
