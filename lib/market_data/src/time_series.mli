(** One sample of a market's price history, normalized across venues.
    Produced by {!Time_series_parser} from venue candlestick/history
    payloads. [no_price] is always the $1 complement of [yes_price]. *)

open! Core
open Types

module Interval : sig
  type t =
    | Minute
    | Hour
    | Day
  [@@deriving sexp]
end

module Point : sig
  type t =
    { time : Time_ns.t
    ; yes_price : Price.t
    ; no_price : Price.t
    }
  [@@deriving sexp_of]
end

type t = Point.t list

val interpolate
  :  t
  -> start:Time_ns.t
  -> finish:Time_ns.t
  -> interval:Interval.t
  -> t Or_error.t

(** The window where every fetched market has data: latest first entry to
    earliest last entry. Errors when a series is empty or the windows do not
    overlap. *)
val shared_window
  :  (Market_stub.t * t) list
  -> (Time_ns.t * Time_ns.t) Or_error.t
