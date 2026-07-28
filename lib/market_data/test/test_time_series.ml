open! Core
open Market_data

let kalshi_time_series = ref None
let polymarket_time_series = ref None

let%expect_test "parse kalshi time series fixture" =
  let body = In_channel.read_all "example_json/kalshi_time_series.json" in
  let points = Time_series_parser.parse_kalshi_time_series body in
  kalshi_time_series := Some points;
  printf "points: %d\n" (List.length points);
  List.iter (List.take points 3) ~f:(fun point ->
    print_s [%sexp (point : Time_series.Point.t)]);
  [%expect
    {|
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
  let body =
    In_channel.read_all "example_json/polymarket_time_series.json"
  in
  let points = Time_series_parser.parse_polymarket_time_series body in
  polymarket_time_series := Some points;
  printf "points: %d\n" (List.length points);
  List.iter (List.take points 3) ~f:(fun point ->
    print_s [%sexp (point : Time_series.Point.t)]);
  [%expect
    {|
    points: 25
    ((time (2025-07-26 00:00:09.000000000Z)) (yes_price 48500000)
     (no_price 51500000))
    ((time (2025-07-26 01:00:10.000000000Z)) (yes_price 49500000)
     (no_price 50500000))
    ((time (2025-07-26 02:00:10.000000000Z)) (yes_price 49500000)
     (no_price 50500000))
    |}]
;;

let%expect_test "sucessfully interpolate kalshi time series" =
  let start = Time_ns.of_string "2026-07-23T02:00:00Z" in
  let finish =
    Time_ns.Span.of_int_sec 1784847600 |> Time_ns.of_span_since_epoch
  in
  let points =
    Option.value_exn !kalshi_time_series
    |> Time_series.interpolate
         ~start
         ~finish
         ~interval:Time_series.Interval.Hour
    |> Or_error.ok_exn
  in
  print_endline [%string "Num points: %{(List.length points)#Int}"];
  List.iter (List.take points 5) ~f:(fun point ->
    print_s [%sexp (point : Time_series.Point.t)]);
  [%expect
    {|
    Num points: 22
    ((time (2026-07-23 02:00:00.000000000Z)) (yes_price 11000000)
     (no_price 89000000))
    ((time (2026-07-23 03:00:00.000000000Z)) (yes_price 11000000)
     (no_price 89000000))
    ((time (2026-07-23 04:00:00.000000000Z)) (yes_price 11000000)
     (no_price 89000000))
    ((time (2026-07-23 05:00:00.000000000Z)) (yes_price 11000000)
     (no_price 89000000))
    ((time (2026-07-23 06:00:00.000000000Z)) (yes_price 11000000)
     (no_price 89000000))
    |}]
;;

let%expect_test "sucessfully interpolate polymarket time series" =
  let start =
    Time_ns.Span.of_int_sec 1753488009 |> Time_ns.of_span_since_epoch
  in
  let finish =
    Time_ns.Span.of_int_sec 1755177666 |> Time_ns.of_span_since_epoch
  in
  let points =
    Option.value_exn !polymarket_time_series
    |> Time_series.interpolate
         ~start
         ~finish
         ~interval:Time_series.Interval.Day
    |> Or_error.ok_exn
  in
  print_endline [%string "Num points: %{(List.length points)#Int}"];
  List.iter (List.take points 5) ~f:(fun point ->
    print_s [%sexp (point : Time_series.Point.t)]);
  [%expect
    {|
    Num points: 20
    ((time (2025-07-26 00:00:09.000000000Z)) (yes_price 48500000)
     (no_price 51500000))
    ((time (2025-07-27 00:00:09.000000000Z)) (yes_price 53500000)
     (no_price 46500000))
    ((time (2025-07-28 00:00:09.000000000Z)) (yes_price 53500000)
     (no_price 46500000))
    ((time (2025-07-29 00:00:09.000000000Z)) (yes_price 53500000)
     (no_price 46500000))
    ((time (2025-07-30 00:00:09.000000000Z)) (yes_price 53500000)
     (no_price 46500000))
    |}]
;;
