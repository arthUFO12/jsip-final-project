open! Core
open Types

let backtestable cards =
  List.filter cards ~f:(fun (card : Protocol.Market_card.t) ->
    card.has_price_history)
;;

let env cards =
  Autocomplete.of_list
    (List.concat_map (backtestable cards) ~f:(fun card ->
       [ card.title; Slug.to_string card.slug ]))
;;

let find cards ~text =
  List.find (backtestable cards) ~f:(fun card ->
    String.Caseless.equal card.title text
    || String.Caseless.equal (Slug.to_string card.slug) text)
;;
