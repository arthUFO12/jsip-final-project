(* Tests for {!Client_logic.Search_index}: what the market search bar can see
   and how clicked suggestions resolve. *)

open! Core
open Types
open Client_logic

let card ?(has_price_history = true) ~slug ~title () : Protocol.Market_card.t
  =
  { slug = Slug.of_string slug
  ; title
  ; category = Miscellaneous
  ; volume = None
  ; has_price_history
  }
;;

let cards =
  [ card ~slug:"kxmars-99" ~title:"Elon visits Mars" ()
  ; card
      ~slug:"dead-market"
      ~title:"No history here"
      ~has_price_history:false
      ()
  ]
;;

let%expect_test "titles and tickers index only backtestable markets" =
  print_s
    [%sexp (Autocomplete.candidates (Search_index.env cards) : string list)];
  [%expect {| ("Elon visits Mars" kxmars-99) |}]
;;

let%expect_test "clicked suggestions resolve by title or ticker" =
  let show text =
    match Search_index.find cards ~text with
    | None -> print_endline "no match"
    | Some card -> print_endline (Slug.to_string card.slug)
  in
  show "elon visits mars";
  show "KXMARS-99";
  show "no history here";
  [%expect {|
    kxmars-99
    kxmars-99
    no match
    |}]
;;
