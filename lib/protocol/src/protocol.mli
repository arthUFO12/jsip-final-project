(** The wire protocol between the Bonsai client and [app/server]: typed
    Async-RPCs carried over the page's websocket. Both sides link this
    library, so requests and responses are bin_prot-typed end to end — there
    is no JSON.

    Everything here must stay linkable under js_of_ocaml: [core],
    [async_rpc_kernel], and {!Types} only — no [Async_unix], no cohttp, no
    database code.

    Wire conventions: times are float seconds since epoch (chart-ready and
    free of [Time_ns] bin_io concerns) and money is float dollars. Both
    conversions happen at the server boundary. *)

open! Core
open Async_rpc_kernel
open Types

(** [Time_ns.t] as wire seconds. *)
val epoch_seconds : Time_ns.t -> float

module Market_card : sig
  (** What the market-browser page needs to render one market — a
      display-focused projection of {!Market_stub.t}. *)
  type t =
    { slug : Slug.t
    ; title : string
    ; category : Category.t
    ; volume : Volume.t option
    ; has_price_history : bool
    (** Only markets with price history can be backtested — the bot builder's
        search offers just these. *)
    }
  [@@deriving sexp_of, bin_io, compare, equal]

  val of_stub : Market_stub.t -> t
end

module Interval : sig
  type t =
    | Minute
    | Hour
    | Day
  [@@deriving sexp_of, bin_io, compare, equal, enumerate]

  val name : t -> string
end

module Basic_bots : sig
  (** The optional baseline group for a simulation: the server also runs this
      many random reference bots ([Bots.Basic]) over the same markets and
      window as the configurable bot, and returns their per-tick metrics
      averaged across the group in [Sim_result.baseline_ticks]. *)
  type t =
    { count : int (** How many random bots to average, 1 to 100. *)
    ; trade_probability : float
    (** Chance each market trades on a given tick, in (0, 1]. *)
    ; max_size : int (** Trades are 1..[max_size] contracts, 1 to 1000. *)
    }
  [@@deriving sexp_of, bin_io]
end

module Sim_request : sig
  type t =
    { slugs : Slug.t list (** 1 to 4 markets the bot trades. *)
    ; program : string (** Bot-language rules, one statement per line. *)
    ; variables : string list
    (** Variable definitions ([lot = 2 + 3]), parsed before [program] so the
        rules can use them. *)
    ; interval : Interval.t
    ; lookback_days : int (** Window is [now - lookback .. now]. *)
    ; warmup_hours : int (** History-only prefix ([sim_start_offset]). *)
    ; starting_cash_cents : int (** The bot's opening cash, in cents. *)
    ; allow_negative_cash : bool
    (** When false, trades that would overdraw cash while growing the
        position are rejected. *)
    ; basic_bots : Basic_bots.t option
    (** [Some] also backtests that many random baseline bots over the same
        resolved window; [None] runs only the configurable bot. *)
    }
  [@@deriving sexp_of, bin_io]
end

module Fill : sig
  type t =
    { time_s : float
    ; id : int
    ; side : Side.t
    ; contract : Contract_type.t
    ; size : int
    ; slug : Slug.t
    ; rejected : string option (** [None] means the fill was accepted. *)
    ; reason : string option
    (** Why the bot fired this action: the rule's qualifiers plus the true
        conditions, e.g. ["every 2h; $cash (120.50) > 50"]. *)
    }
  [@@deriving sexp_of, bin_io]
end

module Tick_point : sig
  (** The bot's book and the marked Yes prices after one tick. *)
  type t =
    { time_s : float
    ; cash : float
    ; realized : float
    ; unrealized : float
    ; yes_prices : (Slug.t * float) list
    ; inventory : (Slug.t * int) list
    (** Signed contracts held per market after this tick (positive long yes,
        negative long no), sorted by slug like [yes_prices]. *)
    }
  [@@deriving sexp_of, bin_io]
end

module Baseline_point : sig
  (** One tick of the baseline group's book, averaged across the group. Times
      align with {!Tick_point.time_s} — every bot in a request walks the same
      tick grid. [portfolio_value] is computed server-side (long Yes
      inventory marks at the Yes price, short at [1 - yes_price]) because
      baseline per-market prices and inventories are not shipped. *)
  type t =
    { time_s : float
    ; cash : float
    ; realized : float
    ; unrealized : float
    ; portfolio_value : float
    }
  [@@deriving sexp_of, bin_io]
end

module Sim_result : sig
  type t =
    { ticks : Tick_point.t list (** Warmup ticks included. *)
    ; fills : Fill.t list
    ; sim_start_s : float (** Warmup ends here; shade charts before it. *)
    ; baseline_ticks : Baseline_point.t list
    (** Averaged baseline metrics per tick; empty when the request's
        [basic_bots] was [None]. Same grid (warmup included) as [ticks]. *)
    ; pnl_percentile : float option
    (** Percentile rank (0..100) of the configurable bot's final total PnL
        (realized + unrealized) among the baseline bots' final total PnLs;
        [None] when the request had no [basic_bots]. *)
    }
  [@@deriving sexp_of, bin_io]

  (** The final book, when the simulation had any ticks at all. *)
  val final : t -> Tick_point.t option
end

(** Current (still-open) markets from the server's database seed, in no
    particular order — grouping and ranking are client concerns. *)
val get_markets : (unit, Market_card.t list Or_error.t) Rpc.Rpc.t

(** Parse and backtest a bot program against live Kalshi history for the
    requested markets. Errors carry parse locations / validation context
    verbatim for display on the rules page. *)
val run_simulation : (Sim_request.t, Sim_result.t Or_error.t) Rpc.Rpc.t
