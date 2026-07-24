open! Core
open! Types

(* Testing for price functions *)

let%expect_test "trivial" =
  print_endline "Hello World!";
  [%expect {| Hello World! |}]
;;

(* Individual: each case fails independently with its own name; easier to
   spot which broke at a glance. *)
let%expect_test "of_dollars_string" =
  let dollars = "0.0500" in
  let dollars_in_microcents = Price.of_dollars_string dollars in
  print_s [%sexp (dollars_in_microcents : Price.t option)];
  [%expect {| (5000000) |}]
;;

(* List: one test to maintain, diff reads as a table, adding a case is one
   line. *)
let%expect_test "of_dollars_string" =
  List.iter
    [ "1"; "0.0010"; "12.34"; ""; "abc"; "1.123456789"; "-0.5" ]
    ~f:(fun s ->
      printf
        "%-12s -> %s\n"
        s
        (Sexp.to_string [%sexp (Price.of_dollars_string s : Price.t option)]));
  [%expect
    {|
    1            -> (100000000)
    0.0010       -> (100000)
    12.34        -> (1234000000)
                 -> ()
    abc          -> ()
    1.123456789  -> ()
    -0.5         -> ()
    |}]
;;
