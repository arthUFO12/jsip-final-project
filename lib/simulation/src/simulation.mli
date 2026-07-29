(** Backtest harness: replays historical prices through a
    {!Bot_interface.Bot} and books its trades against {!Pnl}.

    [run] fetches each stub's series over [Config.start .. Config.finish] at
    [Config.interval], interpolates them onto one shared time grid, then
    walks the grid tick by tick: prices are marked, [update_data]/[on_tick]
    run, the returned actions are applied to the pnl book, and the bot hears
    back via [on_response]. Ticks earlier than
    [Config.start + Config.sim_start_offset] are warmup: the bot's data is
    kept current but [on_tick] is not called. *)

open! Core
open! Async
open Types

(** A bot module packed with its config type exposed... *)
type 'a bot = (module Bot_interface.Bot with type Config.t = 'a)

(** ...and hidden again, so bots with different configs can share a list. *)
type packed_bot = P : 'a bot * 'a -> packed_bot

(** Returns the bot's final book after the last tick. Errors if any series
    cannot be fetched or does not cover the configured range. *)
val run
  :  Market_stub.t list
  -> packed_bot
  -> Action_summary.t Deferred.Or_error.t
