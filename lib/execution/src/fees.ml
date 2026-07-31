open! Core
open Types

let kalshi_taker_fee (price : Price.t) : Price.t =
  let p = Price.to_microcents price in
  let numerator = 7 * p * (Price.microcents_per_dollar - p) in
  let denominator =
    100 * Price.microcents_per_dollar * Price.microcents_per_cent
  in
  let fee_cents = (numerator + denominator - 1) / denominator in
  Price.of_microcents (fee_cents * Price.microcents_per_cent)
;;

(* Adding a venue to {!Venue.t}? The compiler stops here: name its taker fee
   and everything downstream supports it unchanged. *)
let taker_fee (venue : Venue.t) price =
  match venue with
  | Kalshi -> kalshi_taker_fee price
  | Polymarket -> Price.zero
;;
