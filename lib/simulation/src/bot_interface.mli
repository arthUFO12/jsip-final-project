(** The contract a trading bot must satisfy to be driven by {!Simulation}.
    The harness owns time, market data, and pnl; the bot only reacts: it is
    fed data snapshots via [update_data]/[on_tick] and learns the outcome of
    its actions via [on_response]. *)

open! Core
open Types
open Market_data

module type Bot = sig
  module Config : sig
    type t

    val id : t -> int
    val start : t -> Time_ns.t
    val finish : t -> Time_ns.t
    val interval : t -> Time_series.Interval.t

    (** How long after the beginning of the data the simulation begins
        calling {!on_tick}. The data begins at the latest first entry across
        the fetched series, which may be later than [start] when a venue's
        history does not reach back that far. Data before that point is
        warmup, visible to the bot only through {!init_data}. *)
    val sim_start_offset : t -> Time_ns.Span.t

    val initial_cash : t -> Price.t
  end

  module State : sig
    type t

    val forward_time : t -> Time_ns.t -> unit
  end

  module Data : sig
    type t
  end

  val name : string

  (** Build the bot's initial view of the world from the full warmup history,
      keyed by market slug. *)
  val init_data : Time_series.t Slug.Table.t -> Config.t -> Data.t

  (** Fold one tick's points (one {!Time_series.Point.t} per market) into the
      bot's view. Called once per tick, before {!on_tick}. *)
  val update_data
    :  Config.t
    -> Data.t
    -> Time_series.Point.t Slug.Table.t
    -> Data.t

  val create_state : Config.t -> State.t
  val on_start : Config.t -> State.t -> unit
  val on_tick : Config.t -> State.t -> Data.t -> Action.t list

  (** Called after the harness has applied the actions returned by
      {!on_tick}: one {!Action_response.t} per action, plus a summary of the
      bot's book after all of them settled. *)
  val on_response
    :  Config.t
    -> State.t
    -> Action_response.t list
    -> Action_summary.t
    -> unit
end
