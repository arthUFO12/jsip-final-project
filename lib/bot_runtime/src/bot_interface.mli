open! Core
open Types

module type Bot = sig
  module Config : sig
    type t
    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t

    val id : t -> int
  end

  module State : sig
    type t

    val forward_time : t -> Time_ns.t -> unit
    val update_cash : t -> Price.t -> unit
  end

  module Data : sig
    type t
  end

  val name : string
  val fetch_data : State.t -> Data.t
  val create_state : Config.t -> State.t
  val on_start : Config.t -> State.t -> unit
  val on_tick : Config.t -> State.t -> Data.t -> Action.t list
  val on_response : Config.t -> State.t -> Action_response.t list -> unit
end
