(** Executes {!Protocol.run_simulation} requests: validates the request,
    resolves its slugs against the database seed, parses the bot program,
    probes the markets' recent Kalshi history for a shared window, runs
    {!Simulation.Harness.run_recorded} on {!Bots.Configurable}, and projects
    the recording into wire types. When the request carries [basic_bots], the
    same window is also backtested by that many seeded {!Bots.Basic} bots —
    run in parallel domains alongside the configurable bot — and their
    per-tick metrics come back averaged as [baseline_ticks].

    Errors are user-facing: parse errors keep their line context, unknown or
    history-less markets are named, and window problems say which bound
    failed. *)

open! Core
open Async

val run : Protocol.Sim_request.t -> Protocol.Sim_result.t Deferred.Or_error.t
