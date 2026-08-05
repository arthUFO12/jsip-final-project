(** Backtest harness: replays historical prices through a
    {!Bot_interface.Bot} and books its trades against {!Pnl}.

    The caller fetches each stub's series (typically via
    {!Market_data_gateway.fetch_many_ticker_series}) and passes it in — the
    harness itself does no IO, so one fetch can serve both window probing and
    the backtest. [run] interpolates the series onto one shared time grid
    starting at the latest first entry across them — venues (Polymarket
    especially) do not reliably return history all the way back to the
    requested start. It then walks the grid tick by tick: prices are marked,
    [update_data]/[on_tick] run, the returned actions are applied to the pnl
    book, and the bot hears back via [on_response]. Ticks within
    [Config.sim_start_offset] of the start of the data are warmup: the bot's
    data is kept current but [on_tick] is not called. *)

open! Core
open Types
open Market_data

(** A bot module packed with its config type exposed... *)
type 'a bot = (module Bot_interface.Bot with type Config.t = 'a)

(** ...and hidden again, so bots with different configs can share a list. *)
type packed_bot = P : 'a bot * 'a -> packed_bot

(** Returns the bot's final book after the last tick. Errors if the
    [Config.start .. Config.finish] window is not inside every stub's
    [created_time .. close_time] lifetime, or if any series is empty or does
    not cover the configured range.

    [allow_negative_cash] decides whether trades may overdraw the book's
    cash: when false, an action that would leave cash negative while growing
    the position is rejected (the bot hears the reason via [on_response]);
    reducing or closing trades always pass. *)
val run
  :  Market_stub.t list
  -> (Market_stub.t * Time_series.t) list
  -> packed_bot
  -> allow_negative_cash:bool
  -> Action_summary.t Or_error.t

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
      ; inventories : Size.t Slug.Table.t
      (** Signed contracts held per market after this tick's trades (positive
          long Yes, negative long No; entry per market). *)
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

(** [run] with the per-tick history kept instead of discarded — same inputs,
    same error cases, same cash gate. *)
val run_recorded
  :  Market_stub.t list
  -> (Market_stub.t * Time_series.t) list
  -> packed_bot
  -> allow_negative_cash:bool
  -> Recording.t Or_error.t

(** The bot-facing snapshot of a pnl book, as passed to [on_response]. *)
val summarize : Pnl.t -> Action_summary.t
