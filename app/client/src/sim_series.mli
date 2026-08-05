(** Chart series assembly for the backtest results page: one pass over a
    {!Protocol.Sim_result.t} into named [(time, value)] series, portfolio
    value computed via {!Portfolio.value} once per tick, every series already
    cut to the point budget by {!Downsample}. Colors and dashing stay in the
    view layer — this is data only. *)

open! Core

module Series : sig
  type t =
    { name : string
    ; points : (float * float) list
    }
  [@@deriving sexp_of]
end

type t =
  { pnl : Series.t list (** realized / unrealized / total *)
  ; value : Series.t list (** cash / portfolio value / total value *)
  ; prices : Series.t list (** one per market, named by slug *)
  ; inventory : Series.t list (** one per market, named by slug *)
  ; baseline_pnl : Series.t list (** empty without a baseline run *)
  ; baseline_value : Series.t list (** empty without a baseline run *)
  }
[@@deriving sexp_of]

val create : Protocol.Sim_result.t -> max_points:int -> t
