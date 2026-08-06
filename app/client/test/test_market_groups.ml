(* Tests for {!Client_logic.Market_groups}: category grouping, volume
   ranking, per-category truncation, and busiest-category-first ordering. *)

open! Core
open Types
open Client_logic

let card ?volume ~category ~slug () : Protocol.Market_card.t =
  { slug = Slug.of_string slug
  ; title = String.capitalize slug
  ; category
  ; volume = Option.map volume ~f:(fun n -> Protocol.Volume.Contracts n)
  ; has_price_history = true
  }
;;

let show groups =
  List.iter groups ~f:(fun (category, cards) ->
    let cards =
      List.map cards ~f:(fun (card : Protocol.Market_card.t) ->
        let volume =
          match card.volume with
          | None -> "-"
          | Some volume -> Float.to_string (Protocol.Volume.to_float volume)
        in
        [%string "%{card.slug#Slug}(%{volume})"])
      |> String.concat ~sep:" "
    in
    print_endline [%string "%{category#Category}: %{cards}"])
;;

let%expect_test "groups rank by volume and busiest category leads" =
  show
    (Market_groups.group
       ~max_per_category:2
       ~min_per_category:1
       [ card ~category:Sports ~slug:"nba-finals" ~volume:100 ()
       ; card ~category:Crypto ~slug:"btc-100k" ~volume:9_000 ()
       ; card ~category:Sports ~slug:"world-cup" ~volume:5_000 ()
       ; card ~category:Sports ~slug:"local-derby" ~volume:300 ()
       ; card ~category:Crypto ~slug:"eth-flip" ~volume:40 ()
       ]);
  (* Sports keeps its top two of three; Crypto's 9000 beats Sports' 5000 for
     first place on the page. *)
  [%expect
    {|
    Crypto: btc-100k(9000.) eth-flip(40.)
    Sports: world-cup(5000.) local-derby(300.)
    |}]
;;

let%expect_test "missing volume ranks last within its category" =
  show
    (Market_groups.group
       ~max_per_category:3
       ~min_per_category:1
       [ card ~category:Tech ~slug:"quiet-market" ()
       ; card ~category:Tech ~slug:"tiny-market" ~volume:1 ()
       ]);
  [%expect {| Tech: tiny-market(1.) quiet-market(-) |}]
;;

let%expect_test "empty input yields no groups" =
  show (Market_groups.group ~max_per_category:5 ~min_per_category:1 []);
  [%expect {| |}]
;;

let%expect_test "categories with fewer than min_per_category are hidden" =
  show
    (Market_groups.group
       ~max_per_category:5
       ~min_per_category:3
       [ card ~category:Sports ~slug:"nba-finals" ~volume:100 ()
       ; card ~category:Sports ~slug:"world-cup" ~volume:5_000 ()
       ; card ~category:Sports ~slug:"local-derby" ~volume:300 ()
       ; card ~category:Crypto ~slug:"btc-100k" ~volume:9_000 ()
       ; card ~category:Crypto ~slug:"eth-flip" ~volume:40 ()
       ]);
  (* Crypto has only two markets, so despite holding the single biggest
     market it is dropped entirely; Sports qualifies with three. *)
  [%expect {| Sports: world-cup(5000.) local-derby(300.) nba-finals(100.) |}]
;;
