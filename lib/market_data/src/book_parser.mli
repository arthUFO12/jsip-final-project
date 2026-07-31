open! Core
open Types

(** Decodes a raw order-book payload into a {!Binary_book.t}: the best ask on
    each outcome with the size behind it.

    Venue book models differ, and the derivations live here:
    - Kalshi rests every order as a {e bid} on YES or NO, so the ask on one
      outcome is [$1 -] the best bid on the other, at that bid's count. Both
      wire shapes are understood: the current
      {[
        {"orderbook_fp": {"yes_dollars": [["0.05", "12.69"], ..], ..}}
      ]}
      (decimal-string dollars and shares) and the legacy
      {[
        {"orderbook": {"yes": [[5, 12], ..], ..}}
      ]}
      (whole cents and counts).
    - Polymarket (
      {[
        {"bids": [{"price": "0.48", "size": ..}, ..],
        "asks": ..}
      ]}
      ) is the YES token's book: the YES ask is the lowest ask, and the NO
      ask is [$1 -] the highest YES bid (complementary orders cross, so the
      price is executable), at that bid's size. Fractional share sizes are
      floored to whole contracts.

    Returns [Error _] on malformed JSON or an empty side — a book with no
    resting orders on some side cannot price both legs and callers should
    skip the market, not trade a made-up quote. *)

val parse_kalshi_book : string -> Binary_book.t Or_error.t
val parse_polymarket_book : string -> Binary_book.t Or_error.t
