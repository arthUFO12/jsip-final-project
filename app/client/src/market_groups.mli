(** Pure presentation logic for the market-browser page: which markets to
    show and in what order. Kept free of Bonsai so it expect-tests natively;
    the frontend just renders its output.

    {[
      Market_groups.group cards ~max_per_category:5 ~min_per_category:3
    ]}

    groups the cards by {!Types.Category.t} and ranks everything by traded
    volume: cards within a group are highest-volume first (cards with no
    reported volume last), each group keeps at most [max_per_category] cards,
    a category with fewer than [min_per_category] cards is not shown at all,
    and the groups themselves are ordered by their top card's volume so the
    busiest categories lead the page. *)

open! Core
open Types

val group
  :  Protocol.Market_card.t list
  -> max_per_category:int
  -> min_per_category:int
  -> (Category.t * Protocol.Market_card.t list) list
