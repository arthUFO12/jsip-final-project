(** One sample of a market's price history, normalized across venues.
    Produced by {!Time_series_parser} from venue candlestick/history
    payloads. [no_price] is always the $1 complement of [yes_price]. *)

open! Core
open Types

type t =
  { time : Time_ns.t
  ; yes_price : Price.t
  ; no_price : Price.t
  }
[@@deriving sexp_of]
