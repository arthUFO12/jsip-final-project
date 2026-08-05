(* Tests for {!Client_logic.Portfolio} mark-to-market. *)

open! Core
open Types
open Client_logic

let slug = Slug.of_string

let%expect_test "long marks at the yes price, short at one minus it" =
  let value =
    Portfolio.value
      ~inventory:[ slug "aaa", 10; slug "bbb", -4 ]
      ~yes_prices:[ slug "aaa", 0.30; slug "bbb", 0.25 ]
  in
  (* 10 * 0.30 + 4 * (1 - 0.25) = 3 + 3 = 6 *)
  print_s [%sexp (value : float)];
  [%expect {| 6 |}]
;;

let%expect_test "a price with no inventory entry contributes nothing" =
  let value =
    Portfolio.value
      ~inventory:[ slug "aaa", 2 ]
      ~yes_prices:[ slug "aaa", 0.5; slug "zzz", 0.9 ]
  in
  print_s [%sexp (value : float)];
  [%expect {| 1 |}]
;;

let%expect_test "flat and empty books are worth zero" =
  print_s
    [%sexp
      (Portfolio.value
         ~inventory:[ slug "aaa", 0 ]
         ~yes_prices:[ slug "aaa", 0.6 ]
       : float)];
  [%expect {| 0 |}];
  print_s [%sexp (Portfolio.value ~inventory:[] ~yes_prices:[] : float)];
  [%expect {| 0 |}]
;;
