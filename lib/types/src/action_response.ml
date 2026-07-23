open! Core

type t =
  | Action_accepted of
      { action : Action.t
      ; time_stamp : Time_ns.t
      }
  | Action_rejected of
      { action : Action.t
      ; time_stamp : Time_ns.t
      ; reason : string
      }
