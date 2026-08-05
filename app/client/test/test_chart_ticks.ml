(* Tests for {!Client_logic.Chart_ticks} axis placement. *)

open! Core
open Client_logic

let%expect_test "y ticks step on the 1-2-5 ladder" =
  let ticks lo hi =
    print_s
      [%sexp
        (Chart_ticks.y_ticks { Chart.Range.lo; hi } ~max_count:5
         : float list)]
  in
  ticks 0. 100.;
  [%expect {| (0 20 40 60 80 100) |}];
  ticks (-4.3) 15.2;
  [%expect {| (0 5 10 15) |}];
  ticks 0. 0.9;
  [%expect {| (0 0.2 0.4 0.60000000000000009 0.8) |}];
  ticks 0.42 0.44;
  [%expect {| (0.42 0.425 0.43 0.435 0.44) |}]
;;

let%expect_test "degenerate ranges yield no ticks" =
  print_s
    [%sexp
      (Chart_ticks.y_ticks { Chart.Range.lo = 1.; hi = 1. } ~max_count:5
       : float list)];
  [%expect {| () |}]
;;

let%expect_test "time ticks pick day steps and UTC labels for a two-week \
                 span"
  =
  (* Two weeks starting 2026-07-02T00:00Z (a day boundary, so it is also the
     first tick). *)
  let lo = 1_782_950_400. in
  let hi = lo +. (14. *. 86_400.) in
  print_s
    [%sexp
      (Chart_ticks.time_ticks { Chart.Range.lo; hi } ~max_count:4
       : (float * string) list)];
  [%expect
    {| ((1782950400 "Jul 2") (1783555200 "Jul 9") (1784160000 "Jul 16")) |}]
;;

let%expect_test "sub-day spans label hours" =
  let lo = 1_782_950_400. in
  let hi = lo +. (6. *. 3600.) in
  print_s
    [%sexp
      (Chart_ticks.time_ticks { Chart.Range.lo; hi } ~max_count:4
       : (float * string) list)];
  [%expect
    {|
    ((1782950400 "Jul 2 00:00") (1782961200 "Jul 2 03:00")
     (1782972000 "Jul 2 06:00"))
    |}]
;;
