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
  val init_data : Time_series.t Slug.Table.t -> Config.t -> Data.t

  val update_data
    :  Config.t
    -> Data.t
    -> Time_series.Point.t Slug.Table.t
    -> Data.t

  val create_state : Config.t -> State.t
  val on_start : Config.t -> State.t -> unit
  val on_tick : Config.t -> State.t -> Data.t -> Action.t list

  val on_response
    :  Config.t
    -> State.t
    -> Action_response.t list
    -> Action_summary.t
    -> unit
end
