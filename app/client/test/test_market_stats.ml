(* Tests for {!Client_logic.Market_stats}. *)

open! Core
open Client_logic

let%expect_test "trailing_sum keeps only values inside the window" =
  (* Hourly points with a 2-hour window: each sum spans this point and the
     previous one. *)
  let hourly = [ 0., 10.; 3600., 20.; 7200., 30.; 10800., 40. ] in
  Market_stats.trailing_sum ~window_s:7200. hourly
  |> List.iter ~f:(fun (time, sum) -> printf "%.0f -> %.0f\n" time sum);
  [%expect
    {|
    0 -> 10
    3600 -> 30
    7200 -> 50
    10800 -> 70
    |}]
;;

let%expect_test "trailing_sum with a window wider than the data sums it all" =
  Market_stats.trailing_sum ~window_s:86400. [ 0., 1.; 60., 2.; 120., 3. ]
  |> List.iter ~f:(fun (time, sum) -> printf "%.0f -> %.0f\n" time sum);
  [%expect {|
    0 -> 1
    60 -> 3
    120 -> 6
    |}]
;;

let%expect_test "volatility measures jumpiness, not level" =
  (* A steady climb has constant differences: zero spread. *)
  printf "steady %.3f\n" (Market_stats.volatility [ 1.; 2.; 3.; 4.; 5. ]);
  (* Alternating +-1 moves around 100: differences are +1/-1, spread 1. *)
  printf
    "choppy %.3f\n"
    (Market_stats.volatility [ 100.; 101.; 100.; 101.; 100. ]);
  (* Too short to have two differences. *)
  printf "short %.3f\n" (Market_stats.volatility [ 3.; 9. ]);
  [%expect {|
    steady 0.000
    choppy 1.000
    short 0.000
    |}]
;;
