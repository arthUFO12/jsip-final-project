open! Core

(** What one venue's book offers right now on a binary market: the best ask
    on each outcome and how many contracts sit at it. {!Bbo.t} can't serve
    here — it carries no depth, and a sized entry needs to know how many
    contracts the price is good for.

    Venues don't all quote this shape directly; parsers derive it. On Kalshi
    every resting order is a bid on YES or NO, so the ask on one outcome is
    [$1 -] the best bid on the other. On Polymarket the CLOB serves the YES
    token's book: the YES ask is quoted directly and the NO ask is [$1 -] the
    best YES bid (complementary orders cross, so that price is executable). *)
type t =
  { yes_ask : Price.t
  ; yes_ask_size : Size.t
  ; no_ask : Price.t
  ; no_ask_size : Size.t
  }
[@@deriving sexp_of]
