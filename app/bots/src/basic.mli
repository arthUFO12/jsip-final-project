(** A reference bot that trades at random. On each tick, each market it knows
    about trades with probability [trade_probability]: a random side,
    contract type, and size up to [max_size]. Randomness is seeded from the
    config, so a given config always produces the same trades.

    Useful for exercising {!Simulation.Harness} end to end and as the
    smallest example of implementing {!Simulation.Bot_interface.Bot}:

    {[
      Harness.run stubs (Harness.P ((module Basic.Bot), config))
    ]} *)

open! Core
open! Async
open Types
open Market_data
open Simulation

module Config : sig
  type t =
    { id : int
    ; start : Time_ns.t
    ; finish : Time_ns.t
    ; interval : Time_series.Interval.t
    ; sim_start_offset : Time_ns.Span.t
    ; initial_cash : Price.t
    ; trade_probability : float
    (** Chance each market trades on a given tick, in [0, 1]. *)
    ; max_size : int (** Trades are 1..[max_size] contracts. *)
    ; seed : int (** Same seed (and id) -> same random trades. *)
    }
  [@@deriving sexp]

  (** Build a config from live Kalshi data: fetch the latest open markets,
      choose the first three that can serve price history, probe their last
      week of hourly candles, and set [start]/[finish] to the window where
      all three have data. Returns the chosen stubs — pass them straight to
      {!Harness.run} alongside the config. Errors if fewer than three usable
      markets exist or their histories do not overlap. *)
  val create
    :  id:int
    -> seed:int
    -> (t * Market_stub.t list) Deferred.Or_error.t
end

module Bot : Bot_interface.Bot with type Config.t = Config.t
