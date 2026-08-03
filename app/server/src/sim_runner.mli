(** Executes {!Protocol.run_simulation} requests: validates the request,
    resolves its slugs against the database seed, parses the bot program,
    probes the markets' recent Kalshi history for a shared window, runs
    {!Simulation.Harness.run_recorded} on {!Bots.Configurable}, and projects
    the recording into wire types.

    Errors are user-facing: parse errors keep their line context, unknown or
    history-less markets are named, and window problems say which bound
    failed. *)

open! Core
open Async

val run : Protocol.Sim_request.t -> Protocol.Sim_result.t Deferred.Or_error.t
