(* Tests for {!Client_logic.Downsample} min-max bucketing. *)

open! Core
open Client_logic

let print points = print_s [%sexp (points : (float * float) list)]

let%expect_test "short series pass through untouched" =
  let points = [ 0., 1.; 1., 2.; 2., 3. ] in
  print (Downsample.downsample points ~max_points:4);
  [%expect {| ((0 1) (1 2) (2 3)) |}]
;;

let%expect_test "endpoints survive and spikes are kept" =
  (* A flat line with one spike in each half: striding could erase them,
     min-max bucketing must not. *)
  let points =
    List.init 100 ~f:(fun i ->
      let y =
        match i with
        | 25 -> 50.
        | 75 -> -50.
        | _ -> 0.
      in
      Float.of_int i, y)
  in
  print (Downsample.downsample points ~max_points:8);
  [%expect {| ((0 0) (1 0) (25 50) (33 0) (66 0) (75 -50) (99 0)) |}]
;;

let%expect_test "output never exceeds the budget" =
  let points =
    List.init 1000 ~f:(fun i ->
      Float.of_int i, Float.sin (Float.of_int i /. 10.))
  in
  List.iter [ 4; 10; 100; 999 ] ~f:(fun max_points ->
    let length = List.length (Downsample.downsample points ~max_points) in
    print_s [%message (max_points : int) (length : int)]);
  [%expect
    {|
    ((max_points 4) (length 4))
    ((max_points 10) (length 10))
    ((max_points 100) (length 100))
    ((max_points 999) (length 998))
    |}]
;;

let%expect_test "a budget below 4 raises" =
  (match Downsample.downsample [ 0., 0. ] ~max_points:3 with
   | (_ : (float * float) list) -> print_endline "no raise"
   | exception exn -> print_s (Exn.sexp_of_t exn));
  [%expect {| ("downsample budget too small" (max_points 3)) |}]
;;
