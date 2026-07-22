open! Core
open Async
open Types
open Database

let real_id = Market_id.of_string "12345"
let fake_id = Market_id.of_string "23456"

let make_stub ~venue ~market_id ~title ~close_time : Market_stub.t =
  { venue = Venue.of_string venue
  ; market_id
  ; title
  ; close_time = Some (Time_ns.of_string close_time)
  }
;;

let%expect_test "successfully connect to database" =
  (match Database_exec.init_database () with
   | Ok () -> print_endline "Database connection is online!"
   | Error e -> print_endline (Error.to_string_hum e));
  return [%expect {| Database connection is online! |}]
;;

let%expect_test "market stub table is successfully created" =
  let%bind success_or_error = Database_exec.create_market_stub_table () in
  (match success_or_error with
   | Ok () -> print_endline "File should be created"
   | Error e -> print_endline (Error.to_string_hum e));
  return [%expect {| File should be created |}]
;;

let%expect_test "market stub is successfully inserted into the table" =
  let stub =
    make_stub
      ~venue:"Polymarket"
      ~market_id:real_id
      ~title:"Don trump tweets"
      ~close_time:"2026-07-20T15:45:00Z"
  in
  let%bind success_or_error = Database_exec.insert_market_stub stub in
  (match success_or_error with
   | Ok () -> print_endline "row successfully created"
   | Error e -> print_endline (Error.to_string_hum e));
  return [%expect {| row successfully created |}]
;;


let%expect_test "market stub is successfully found" =
  let%bind stub_option_or_error = Database_exec.find_market_stub real_id in
  (match stub_option_or_error with
   | Ok stub_option -> 
    (match stub_option with 
    | None -> print_endline "market id not found" 
    | Some stub -> print_endline [%string "Stub found: %{stub#Market_stub}"])
   | Error e -> print_endline (Error.to_string_hum e));
  return [%expect {| Stub found: venue: Polymarket, market_id: 12345, title: Don trump tweets, close_time: 2026-07-20 15:45:00.000000000Z |}]
;;

let%expect_test "Nonexistent market ID is not found" =
  let%bind stub_option_or_error = Database_exec.find_market_stub fake_id in
  (match stub_option_or_error with
   | Ok stub_option -> 
    (match stub_option with 
    | None -> print_endline "market id not found" 
    | Some stub -> print_endline [%string "Stub found: %{stub#Market_stub}"])
   | Error e -> print_endline (Error.to_string_hum e));
  return [%expect {| market id not found |}]
;;