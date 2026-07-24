open! Core
open Market_data
open Types

let%expect_test "parse kalshi fixture" =
  let body = In_channel.read_all "example_json/kalshi_markets.json" in
  match Exchange_parser.parse_data ~body ~venue:Venue.Kalshi with
  | Error e -> print_s [%sexp (e : Error.t)]
  | Ok markets ->
    printf "parsed: %d\n" (List.length markets);
    List.iter (List.take markets 3) ~f:(fun m ->
      print_s [%sexp (m : L1_market_metadata.t)]);
    [%expect
      {|
      parsed: 100
      ((venue Kalshi) (market_id KXWTAMATCH-26JUL25VALSNI-VAL)
       (title
        "Will Tereza Valentova win the Valentova vs Snigur: Semifinal match?")
       (slug KXWTAMATCH-26JUL25VALSNI-VAL) (event_slug (KXWTAMATCH-26JUL25VALSNI))
       (category ()) (yes_bid (8000000)) (yes_ask (93000000))
       (last_price (93000000)) (active true)
       (close_time ((2026-08-08 11:30:00.000000000Z))))
      ((venue Kalshi) (market_id KXWTAMATCH-26JUL25VALSNI-SNI)
       (title "Will Daria Snigur win the Valentova vs Snigur: Semifinal match?")
       (slug KXWTAMATCH-26JUL25VALSNI-SNI) (event_slug (KXWTAMATCH-26JUL25VALSNI))
       (category ()) (yes_bid (9000000)) (yes_ask (93000000)) (last_price (0))
       (active true) (close_time ((2026-08-08 11:30:00.000000000Z))))
      ((venue Kalshi) (market_id KXCLUBFGAME-26JUL25DUBKUS-TIE)
       (title "Dubrava Zagreb vs Kustosija Zagreb Winner?")
       (slug KXCLUBFGAME-26JUL25DUBKUS-TIE)
       (event_slug (KXCLUBFGAME-26JUL25DUBKUS)) (category ()) (yes_bid (12000000))
       (yes_ask (74000000)) (last_price (0)) (active true)
       (close_time ((2026-08-08 08:00:00.000000000Z))))
      |}]
;;

let%expect_test "parse polymarket fixture" =
  let body = In_channel.read_all "example_json/polymarket_markets.json" in
  match Exchange_parser.parse_data ~body ~venue:Venue.Polymarket with
  | Error e -> print_s [%sexp (e : Error.t)]
  | Ok markets ->
    printf "parsed: %d\n" (List.length markets);
    List.iter (List.take markets 3) ~f:(fun m ->
      print_s [%sexp (m : L1_market_metadata.t)]);
    [%expect
      {|
      parsed: 100
      ((venue Polymarket)
       (market_id
        0x1fad72fae204143ff1c3035e99e7c0f65ea8d5cd9bd1070987bd1a3316f772be)
       (title "New Rihanna Album before GTA VI?")
       (slug new-rhianna-album-before-gta-vi-926)
       (event_slug (what-will-happen-before-gta-vi)) (category ())
       (yes_bid (50000000)) (yes_ask (53000000)) (last_price (53000000))
       (active true) (close_time ((2026-07-31 12:00:00.000000000Z))))
      ((venue Polymarket)
       (market_id
        0x50ddb9cd80d5c271664a2ebb7fcaed1d0a148d82c8e8d314d830f75a944c3dcc)
       (title "New Playboi Carti Album before GTA VI?")
       (slug new-playboi-carti-album-before-gta-vi-421)
       (event_slug (what-will-happen-before-gta-vi)) (category ())
       (yes_bid (50000000)) (yes_ask (51000000)) (last_price (51000000))
       (active true) (close_time ((2026-07-31 12:00:00.000000000Z))))
      ((venue Polymarket)
       (market_id
        0x32b09f6390252b37d674501527e709016d55581b2c1e544bd4b8167f5f732f4c)
       (title "Will Jesus Christ return before GTA VI?")
       (slug will-jesus-christ-return-before-gta-vi-665)
       (event_slug (what-will-happen-before-gta-vi)) (category ())
       (yes_bid (49000000)) (yes_ask (50000000)) (last_price (49000000))
       (active true) (close_time ((2026-07-31 12:00:00.000000000Z))))
      |}]
;;
