open! Core
open Types

(** Taker fees per venue — one source of truth shared by the arbitrage
    detector (pricing an edge) and the executors (charging a fill). A fee
    change lands here and both sides move together. All fees are per
    contract; multiply by the fill count for a total. *)

(** Kalshi's taker fee for one contract bought at this price:
    [ceil (0.07 * P * (1 - P))], rounded up to a whole cent. *)
val kalshi_taker_fee : Price.t -> Price.t

(** The taker fee one venue charges for one contract at this price.
    Polymarket charges none today. *)
val taker_fee : Venue.t -> Price.t -> Price.t
