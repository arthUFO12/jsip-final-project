open! Core
open Market_data

let%expect_test "parse kalshi time series fixture" =
  let body = In_channel.read_all "example_json/kalshi_time_series.json" in
  let points = Time_series_parser.parse_kalshi_time_series body in
  printf "points: %d\n" (List.length points);
  List.iter (List.take points 3) ~f:(fun point ->
    print_s [%sexp (point : Time_series_point.t)]);
  [%expect {|
    points: 5
    ((time (2026-07-23 02:00:00.000000000Z)) (yes_price 11000000)
     (no_price 89000000))
    ((time (2026-07-23 11:00:00.000000000Z)) (yes_price 11500000)
     (no_price 88500000))
    ((time (2026-07-23 13:00:00.000000000Z)) (yes_price 11500000)
     (no_price 88500000))
    |}]
;;

let%expect_test "parse polymarket time series fixture" =
  let body = In_channel.read_all "example_json/polymarket_time_series.json" in
  let points = Time_series_parser.parse_polymarket_time_series body in
  printf "points: %d\n" (List.length points);
  List.iter (List.take points 3) ~f:(fun point ->
    print_s [%sexp (point : Time_series_point.t)]);
  [%expect {|
    points: 25
    ((time (2025-07-26 00:00:09.000000000Z)) (yes_price 48500000)
     (no_price 51500000))
    ((time (2025-07-26 01:00:10.000000000Z)) (yes_price 49500000)
     (no_price 50500000))
    ((time (2025-07-26 02:00:10.000000000Z)) (yes_price 49500000)
     (no_price 50500000))
    |}]
;;
