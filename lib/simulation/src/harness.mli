(** Backtest harness: replays historical prices through a
    {!Bot_interface.Bot} and books its trades against {!Pnl}.

    [run] fetches each stub's series over [Config.start .. Config.finish] at
    [Config.interval], then interpolates them onto one shared time grid
    starting at the latest first entry across the series — venues (Polymarket
    especially) do not reliably return history all the way back to the
    requested start. It then walks the grid tick by tick: prices are marked,
    [update_data]/[on_tick] run, the returned actions are applied to the pnl
    book, and the bot hears back via [on_response]. Ticks within
    [Config.sim_start_offset] of the start of the data are warmup: the bot's
    data is kept current but [on_tick] is not called. *)

open! Core
open! Async
open Types

(** A bot module packed with its config type exposed... *)
type 'a bot = (module Bot_interface.Bot with type Config.t = 'a)

(** ...and hidden again, so bots with different configs can share a list. *)
type packed_bot = P : 'a bot * 'a -> packed_bot

(** Returns the bot's final book after the last tick. Errors — before any
    data is fetched — if the [Config.start .. Config.finish] window is not
    inside every stub's [created_time .. close_time] lifetime, and afterwards
    if any series cannot be fetched or does not cover the configured range. *)
val run
  :  Market_stub.t list
  -> packed_bot
  -> Action_summary.t Deferred.Or_error.t

(** Everything a caller needs to replay a finished simulation: the book after
    every tick (warmup included), every action response in fill order, and
    where warmup ended. *)
module Recording : sig
  module Tick : sig
    type t =
      { time : Time_ns.t
      ; cash : Price.t
      ; realized_pnl : Price.t
      ; unrealized_pnl : Price.t
      ; yes_prices : Price.t Slug.Table.t
      (** Marked Yes price per market on this tick. *)
      }
  end

  type t =
    { ticks : Tick.t list
    ; responses : Action_response.t list
    ; sim_start : Time_ns.t
    (** First non-warmup tick; earlier ticks carry prices but no trading. *)
    ; summary : Action_summary.t
    }
end

(** [run] with the per-tick history kept instead of discarded — same
    fetching, same error cases. *)
val run_recorded
  :  Market_stub.t list
  -> packed_bot
  -> Recording.t Deferred.Or_error.t

(** The bot-facing snapshot of a pnl book, as passed to [on_response]. *)
val summarize : Pnl.t -> Action_summary.t
