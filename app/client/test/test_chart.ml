(* Tests for {!Client_logic.Chart} geometry. *)

open! Core
open Client_logic

let%expect_test "polyline maps data into the pixel box, y inverted" =
  let points = [ 0., 0.; 1., 5.; 2., 10. ] in
  let x_range : Chart.Range.t = { lo = 0.; hi = 2. } in
  let y_range : Chart.Range.t = { lo = 0.; hi = 10. } in
  print_endline
    (Chart.polyline points ~x_range ~y_range ~width:100. ~height:50.);
  (* Larger y = higher on screen = smaller SVG y. *)
  [%expect {| 0.0,50.0 50.0,25.0 100.0,0.0 |}]
;;

let%expect_test "ranges pad and widen degenerate data" =
  let { Chart.Range.lo; hi } = Chart.Range.of_values [ 0.; 10. ] in
  print_s [%message "" (lo : float) (hi : float)];
  [%expect {| ((lo -0.5) (hi 10.5)) |}];
  let { Chart.Range.lo; hi } = Chart.Range.of_values [ 0.4; 0.4 ] in
  print_s [%message "" (lo : float) (hi : float)];
  [%expect {| ((lo -0.099999999999999978) (hi 0.9)) |}];
  let { Chart.Range.lo; hi } = Chart.Range.of_values [] in
  print_s [%message "" (lo : float) (hi : float)];
  [%expect {| ((lo -0.5) (hi 0.5)) |}]
;;

let%expect_test "labels are compact" =
  List.iter [ 1.25; -0.1; 120. ] ~f:(fun value ->
    print_endline (Chart.label value));
  [%expect {|
    1.25
    -0.10
    120.00
    |}]
;;

let%expect_test "unscale_x inverts scale_x" =
  let range : Chart.Range.t = { lo = 10.; hi = 30. } in
  List.iter [ 10.; 17.5; 30. ] ~f:(fun value ->
    let pixel = Chart.scale_x range ~extent:640. value in
    let round_trip = Chart.unscale_x range ~extent:640. pixel in
    print_s [%message (value : float) (pixel : float) (round_trip : float)]);
  [%expect
    {|
    ((value 10) (pixel 0) (round_trip 10))
    ((value 17.5) (pixel 240) (round_trip 17.5))
    ((value 30) (pixel 640) (round_trip 30))
    |}]
;;

let%expect_test "nearest snaps to the closest x, ends included" =
  let points = [| 0., 10.; 5., 20.; 10., 30. |] in
  List.iter [ -3.; 2.4; 2.6; 5.; 9.; 40. ] ~f:(fun x ->
    print_s
      [%sexp (Chart.nearest points ~x : (float * float) option)]);
  [%expect
    {|
    ((0 10))
    ((0 10))
    ((5 20))
    ((5 20))
    ((10 30))
    ((10 30))
    |}];
  print_s [%sexp (Chart.nearest [||] ~x:1. : (float * float) option)];
  [%expect {| () |}]
;;
