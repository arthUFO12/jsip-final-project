open! Core
open! Types

(* Testing for price functions *)

let%expect_test "trivial" =
  print_endline "Hello World!";
  [%expect {| Hello World! |}]
;;

let%expect_test "of_dollars_string" =
  let dollars = "0.0500" in
  let dollars_in_microcents = Price.of_dollars_string dollars in
  print_s [%sexp (dollars_in_microcents : Price.t option)];
  [%expect {| |}]
;;
