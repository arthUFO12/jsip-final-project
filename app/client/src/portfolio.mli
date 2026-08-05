(** Mark-to-market value of a signed inventory, in dollars: what selling
    every held contract at the marked Yes prices would fetch. Long Yes
    marks at the price, short (negative) at [1 - price]. A price with no
    inventory entry contributes nothing — the wire lists are sorted alike
    but this does not assume they align. *)

open! Core
open Types

val value
  :  inventory:(Slug.t * int) list
  -> yes_prices:(Slug.t * float) list
  -> float
