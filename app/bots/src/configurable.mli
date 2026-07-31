(** A bot driven by a user-written rule {!Parser.Program} instead of
    hard-coded logic. Each tick it evaluates every rule whose qualifiers pass
    (EVERY on the simulated-tick grid, WHEN-I on the previous tick's accepted
    fills) against a snapshot of its price history and latest pnl, and emits
    the resulting actions. Rejected actions are logged and otherwise ignored
    — the simulation continues.

    Unlike {!Basic}, whose config fetches live markets, [Config.create] here
    is network-free: the caller supplies the simulation window and slugs
    (e.g. from {!Basic.Config.create}-style market selection) plus the rules,
    and all rule validation happens up front via {!Parser.Program.create}. *)

open! Core
open Types
open Market_data
open Simulation

module Config : sig
  type t

  val create
    :  id:int
    -> start:Time_ns.t
    -> finish:Time_ns.t
    -> interval:Time_series.Interval.t
    -> sim_start_offset:Time_ns.Span.t
    -> initial_cash:Price.t
    -> slugs:Slug.t list
    -> rules:Parser.Rule.t list
    -> t Or_error.t
end

module Bot : Bot_interface.Bot with type Config.t = Config.t
