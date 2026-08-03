open! Core
open Market_data
open Types

let%expect_test "parse kalshi fixture" =
  let body = In_channel.read_all "example_json/kalshi_events.json" in
  match Live_data_parser.parse_data ~venue:Venue.Kalshi body with
  | Error e -> print_s [%sexp (e : Error.t)]
  | Ok markets ->
    printf "parsed: %d\n" (List.length markets);
    List.iter markets ~f:(fun m ->
      print_s [%sexp (m : L1_market_metadata.t)]);
    [%expect
      {|
      parsed: 1
      ((venue Kalshi) (market_id KXELONMARS-99)
       (title "Will Elon Musk visit Mars before Aug 1, 2099?") (slug KXELONMARS-99)
       (event_slug (KXELONMARS-99)) (series_ticker (KXELONMARS)) (clob_token_id ())
       (category Miscellaneous) (yes_bid (12000000)) (yes_ask (13000000))
       (last_price (13000000)) (volume ((Contracts 114510)))
       (volume_24h ((Contracts 43))) (active true)
       (created_time (2025-08-28 20:40:16.168768000Z))
       (close_time (2099-08-01 04:59:00.000000000Z)))
      |}]
;;

let%expect_test "parse polymarket fixture" =
  let body = In_channel.read_all "example_json/polymarket_markets.json" in
  match Live_data_parser.parse_data ~venue:Venue.Polymarket body with
  | Error e -> print_s [%sexp (e : Error.t)]
  | Ok markets ->
    printf "parsed: %d\n" (List.length markets);
    List.iter markets ~f:(fun m ->
      print_s [%sexp (m : L1_market_metadata.t)]);
    [%expect
      {|
      parsed: 1
      ((venue Polymarket)
       (market_id
        0x1fad72fae204143ff1c3035e99e7c0f65ea8d5cd9bd1070987bd1a3316f772be)
       (title "New Rihanna Album before GTA VI?")
       (slug new-rhianna-album-before-gta-vi-926)
       (event_slug (what-will-happen-before-gta-vi)) (series_ticker ())
       (clob_token_id
        (98022490269692409998126496127597032490334070080325855126491859374983463996227))
       (category Politics) (yes_bid (50000000)) (yes_ask (51000000))
       (last_price (51000000)) (volume ((Notional 87713660105302)))
       (volume_24h ((Notional 304691306800))) (active true)
       (created_time (2025-05-02 15:04:43.762151000Z))
       (close_time (2026-07-31 12:00:00.000000000Z)))
      |}]
;;

let%expect_test "parse_data limit caps returned metadatas" =
  let body = In_channel.read_all "example_json/polymarket_markets.json" in
  match
    Live_data_parser.parse_data ~limit:0 ~venue:Venue.Polymarket body
  with
  | Error e -> print_s [%sexp (e : Error.t)]
  | Ok markets ->
    printf "parsed: %d\n" (List.length markets);
    [%expect {| parsed: 0 |}]
;;

let%expect_test "parse polymarket search fixture: markets flattened, event \
                 tags drive the category"
  =
  let body = In_channel.read_all "example_json/polymarket_search.json" in
  match Live_data_parser.parse_polymarket_search body with
  | Error e -> print_s [%sexp (e : Error.t)]
  | Ok markets ->
    List.iter markets ~f:(fun (m : L1_market_metadata.t) ->
      print_endline
        [%string
          "%{m.category#Category} | %{m.title} | active: %{m.active#Bool}"]);
    [%expect
      {|
      Crypto | Will the price of Bitcoin be less than $120K on July 15 at 5PM ET? | active: false
      Crypto | Will the price of Bitcoin be between $120K and $121K on July 15 at 5PM ET? | active: false
      |}]
;;
