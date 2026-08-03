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
