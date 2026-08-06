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

module Volume : sig
  (** Traded volume, mirrored from {!Types.Volume} as a wire type: that
      type's [Notional] payload is microcents in a 63-bit int, which
      overflows js_of_ocaml's 32-bit client ints — so dollars ride the wire
      as floats. The constructor still records the venue's unit (Kalshi
      counts contracts, Polymarket reports USD notional). *)
  type t =
    | Contracts of int
    | Notional_dollars of float
  [@@deriving sexp_of, bin_io, compare, equal]

  val of_venue_volume : Types.Volume.t -> t

  (** The bare magnitude, for ranking markets by activity — only meaningful
      as an ordering key, the units differ across constructors. *)
  val to_float : t -> float
end

module Market_card : sig
  (** What the market-browser page needs to render one market — a
      display-focused projection of {!Market_stub.t}. *)
  type t =
    { slug : Slug.t
    ; title : string
    ; category : Category.t
    ; volume : Volume.t option
    ; has_price_history : bool
    (** The venue can serve this market's price history
        ({!Market_stub.can_fetch_history}), so it can be charted in the
        detail popup and backtested. Polymarket history carries no per-candle
        volume, so its popup shows no volume chart. *)
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

module Market_detail : sig
  (** Price history and per-candle traded volume for one market, fetched live
      for the market-detail popup. [volumes] is empty when the venue reports
      none (Polymarket). Times are epoch seconds; prices dollars; volumes
      contracts traded per candle. *)
  type t =
    { prices : (float * float) list
    ; volumes : (float * float) list
    }
  [@@deriving sexp_of, bin_io]
end

module Basic_bots : sig
  (** A dumb-bot baseline group for a simulation: the server also runs this
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

module Rival_bot : sig
  (** A user-authored baseline: a previously saved bot re-run as a group of
      one. It trades its own [slugs] with its own rules, but shares the
      request's interval, window, starting cash, and cash policy so its
      series line up with the configurable bot's. *)
  type t =
    { name : string (** Display name, echoed in error messages. *)
    ; slugs : Slug.t list (** 1 to 4 markets the rival trades. *)
    ; program : string (** Bot-language rules, one statement per line. *)
    ; variables : string list
    }
  [@@deriving sexp_of, bin_io]
end

module Comparison : sig
  (** What to backtest alongside the configurable bot for
      [Sim_result.baseline_ticks]. A variant rather than two options: the
      baseline modes are mutually exclusive by construction. *)
  type t =
    | No_comparison
    | Dumb_bots of Basic_bots.t
    | Rival_bot of Rival_bot.t
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
    ; comparison : Comparison.t
    (** The baseline to backtest over the same resolved window, if any. *)
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
    (** Averaged baseline metrics per tick (a {!Comparison.Rival_bot} is a
        group of one); empty when the request's [comparison] was
        [No_comparison]. Same grid (warmup included) as [ticks]. *)
    ; pnl_percentile : float option
    (** Percentile rank (0..100) of the configurable bot's final total PnL
        (realized + unrealized) among the baseline bots' final total PnLs;
        [None] unless the request compared against [Dumb_bots] — a rank
        within a group of one rival says nothing. *)
    ; truncated : bool
    (** True when the requested lookback reached before the venue's available
        history, so the simulation starts later than asked. *)
    }
  [@@deriving sexp_of, bin_io]

  (** The final book, when the simulation had any ticks at all. *)
  val final : t -> Tick_point.t option
end

module Pair_status : sig
  (** The review lifecycle of a cross-venue pair, mirrored from
      [Types.Pair_proposal.Status] as a wire type so this library stays free
      of bin_io obligations on {!Types}. The server converts at its boundary. *)
  type t =
    | Proposed
    | Approved
    | Rejected
  [@@deriving sexp_of, bin_io, compare, equal, enumerate]

  val name : t -> string
end

module Pair_card : sig
  (** One matched cross-venue pair as the review page renders it: both titles
      with their venues, the text-matcher's score, the LLM's explanation when
      one adjudicated, and where it sits in the review lifecycle. [index] is
      the handle {!decide_pair} accepts, valid for the listing it appeared
      in. *)
  type t =
    { index : int
    ; left_title : string
    ; left_venue : string
    ; right_title : string
    ; right_venue : string
    ; score : float
    ; explanation : string option
    ; status : Pair_status.t
    }
  [@@deriving sexp_of, bin_io]
end

module Sweep_request : sig
  (** Text-only matching at [threshold] — the UI never spends LLM credits;
      adjudicated sweeps stay on the CLI where the flag is explicit. *)
  type t = { threshold : float } [@@deriving sexp_of, bin_io]
end

module Sweep_summary : sig
  (** What one sweep did: listings compared, cross-venue candidates the
      matcher saw, and how many survived to the review queue. *)
  type t =
    { markets_swept : int
    ; search_hits : int
    ; proposed : int
    }
  [@@deriving sexp_of, bin_io]
end

module Decide_request : sig
  (** Move the [index]th pair of the [tab] listing — the same numbering
      {!get_pairs} just returned for that status — to [new_status]. This is
      also the undo path: a pair the LLM (or a hasty click) filed under
      Approved or Rejected can be pulled back out from its tab. *)
  type t =
    { tab : Pair_status.t
    ; index : int
    ; new_status : Pair_status.t
    }
  [@@deriving sexp_of, bin_io]
end

module Llm_review_request : sig
  (** [api_key] is the caller's own Anthropic key; [None] falls back to the
      server's [ANTHROPIC_API_KEY], if set. The key is used for this batch
      and never stored. *)
  type t = { api_key : string option } [@@deriving sexp_of, bin_io]
end

module Auto_review_request : sig
  (** The deterministic gate: approve every Proposed pair whose text-match
      score is at least [threshold]. No model, no tokens — and no judgment:
      it trusts the text matcher that filed the pairs completely. *)
  type t = { threshold : float } [@@deriving sexp_of, bin_io]
end

module Auto_review_summary : sig
  (** One deterministic pass: [reviewed] entries read, [approved] at or above
      the threshold, [left_proposed] below it (untouched, for a human or the
      LLM). *)
  type t =
    { reviewed : int
    ; approved : int
    ; left_proposed : int
    }
  [@@deriving sexp_of, bin_io]
end

module Llm_review_summary : sig
  (** One LLM pass over the Proposed queue: how many pairs it read, where
      they landed, and — when calls failed (bad key, network) — how many,
      with the first error verbatim for display. *)
  type t =
    { reviewed : int
    ; approved : int
    ; rejected : int
    ; errored : int
    ; first_error : string option
    }
  [@@deriving sexp_of, bin_io]
end

module Edge_leg : sig
  (** One side of a priced split: buy this pair's YES (or NO) on [venue] at
      [ask] dollars. [url] is the venue's own page for the market — the
      "actually go do it" link. *)
  type t =
    { venue : string
    ; title : string
    ; ask : float
    ; url : string
    }
  [@@deriving sexp_of, bin_io]
end

module Edge_card : sig
  (** One approved pair priced against both live books: the cheaper split's
      legs, its combined [cost] (asks plus taker fees, dollars), the [edge]
      left under $1 (negative when the pair costs more than it pays), depth
      as [size] contracts, and whether the bot would act ([tradable] means
      the edge clears its minimum and has size behind it). [dollars] is
      [edge * size] — what taking the whole opportunity would bank, fees
      included; tradable cards are booked into the wallet under [pair_key],
      and [acted] says the user already marked this pair really-traded. *)
  type t =
    { pair_key : string
    ; yes : Edge_leg.t
    ; no : Edge_leg.t
    ; cost : float
    ; edge : float
    ; size : int
    ; dollars : float
    ; tradable : bool
    ; acted : bool
    }
  [@@deriving sexp_of, bin_io]
end

module Venue_error : sig
  (** Execution failures as data, not strings — different failures demand
      different UI (a geo-block is sticky; a rate limit is retryable). *)
  type t =
    | Geo_blocked
    | Insufficient_funds
    | Market_closed_or_halted
    | Rate_limited
    | Other of string
  [@@deriving sexp_of, bin_io, equal]
end

module Execution_capability : sig
  (** Whether this server can place real orders at all — and why not, when it
      can't ([reason]: no [-allow-live], missing credentials, or the kill
      switch). The UI hides every execution affordance unless [live]. *)
  type t =
    { live : bool
    ; reason : string option
    }
  [@@deriving sexp_of, bin_io]
end

module Preflight_request : sig
  (** [manual_is_yes] names the side the user intends to take manually (known
      from the edge card's split), so the server can price the opposite
      contract's hedge. *)
  type t =
    { pair_key : string
    ; manual_is_yes : bool
    }
  [@@deriving sexp_of, bin_io]
end

module Preflight : sig
  (** The advisory numbers assisted execution's step 1 shows before the user
      commits money to their manual leg: the rails checked in advance.
      [hedgeable] is the bottom line — contracts the app could hedge right
      now given the kill switch, caps room, and book depth. Advisory only;
      the authoritative check re-runs at execution. *)
  type t =
    { kill_switch : string option (** [Some reason] when tripped *)
    ; hedge_title : string (** the Kalshi market that would be hedged *)
    ; hedge_is_yes : bool (** which contract the hedge would buy *)
    ; ask_cents : int (** current ask for that contract *)
    ; depth : int (** contracts available at that ask *)
    ; caps_room_dollars : float (** spend room left under the day cap *)
    ; hedgeable : int
    }
  [@@deriving sexp_of, bin_io]
end

module Hedge_request : sig
  (** Step 2's confirmation: what the user says they did on the manual venue,
      and the ceiling the app may pay to hedge it. [nonce] is minted once at
      dialog-confirm time and derives the venue [client_order_id], so
      re-sending the same confirmation after a timeout cannot double-hedge.
      [manual_is_yes] names the side the user took; the app hedges the
      opposite contract on Kalshi. *)
  type t =
    { pair_key : string
    ; nonce : string
    ; manual_venue : string
    ; manual_is_yes : bool
    ; manual_price_cents : int
    ; manual_count : int
    ; max_hedge_price_cents : int
    }
  [@@deriving sexp_of, bin_io]
end

module Hedge_result : sig
  (** Three-armed on purpose: money can move without success. [Hedged] with
      [unhedged > 0] is a partial — the user is still one-sided by that many
      contracts. [Refused] means nothing was sent (rails or slippage guard);
      retry is safe. [Failed] means the venue was touched and the position is
      one-sided by [unhedged]. *)
  type t =
    | Hedged of
        { price : float
        ; count : int
        ; fee : float
        ; unhedged : int
        }
    | Refused of string
    | Failed of
        { error : Venue_error.t
        ; detail : string
        ; unhedged : int
        }
  [@@deriving sexp_of, bin_io]
end

module Wallet_card : sig
  (** One booked opportunity as the wallet panel renders it. *)
  type t =
    { pair_key : string
    ; summary : string
    ; dollars : float
    ; acted : bool
    ; acted_dollars : float
    }
  [@@deriving sexp_of, bin_io]
end

module Wallet : sig
  (** The "for fun" score: [paper] sums every booked opportunity's
      would-have-made dollars (fees included) as if the user had acted on all
      of them; [acted] sums only the entries they marked as really traded. *)
  type t =
    { paper : float
    ; acted : float
    ; entries : Wallet_card.t list
    }
  [@@deriving sexp_of, bin_io]
end

module Scan_report : sig
  (** One detection tick over the approved pairs, as {!Arbitrage.Bot} would
      run it: how many pairs were read from the store, how many legs got a
      live book, and each pair's pricing. *)
  type t =
    { pairs : int
    ; legs_priced : int
    ; edges : Edge_card.t list
    ; tradable : int
    }
  [@@deriving sexp_of, bin_io]
end

module Trading_key : sig
  (** The web half of the connect-your-wallet flow. The browser sends venue
      credentials exactly once, in {!Connect_request} over the localhost
      socket; the server encrypts them at rest ({!Execution.Wallet_store})
      and everything it ever sends back is a {!Status} — which carries a
      recognition hint, never the key id in full and never the PEM. *)

  module Status : sig
    type t =
      { connected : bool (** an encrypted key file exists on the server *)
      ; unlocked : bool
      (** credentials are decrypted in server memory; live executor installed *)
      ; key_hint : string option
      (** e.g. ["cc2a..5e86"] — enough to recognize, never the whole id *)
      ; production : bool
      (** the stored key targets the real-money host, not demo *)
      ; live_allowed : bool
      (** the server was started with [-allow-live]; without it, unlock
          refuses and the UI says why *)
      }
    [@@deriving sexp_of, bin_io]
  end

  module Connect_request : sig
    (** The one message that carries secrets, browser → server only.
        [production] chooses the host the key will sign for — demo unless the
        user explicitly says otherwise. *)
    type t =
      { key_id : string
      ; private_key_pem : string
      ; passphrase : string
      ; production : bool
      }
    [@@deriving sexp_of, bin_io]
  end
end

module Account : sig
  (** The unlocked trading key's account as the venue reports it — cash and
      open positions on whichever host (demo or production) the key targets.
      Only as fresh as the venue's read side. *)

  module Position : sig
    type t =
      { ticker : string
      ; position : int
      (** Signed contracts: positive long YES, negative NO. *)
      ; exposure_dollars : float (** The venue's marked exposure. *)
      }
    [@@deriving sexp_of, bin_io]
  end

  type t =
    { balance_dollars : float
    ; positions : Position.t list
    ; production : bool (** Real dollars, not demo. *)
    }
  [@@deriving sexp_of, bin_io]
end

(** Current (still-open) markets from the server's database seed, in no
    particular order — grouping and ranking are client concerns. *)
val get_markets : (unit, Market_card.t list Or_error.t) Rpc.Rpc.t

(** Live price (and, for Kalshi, volume) history for one market since its
    creation — hourly candles for markets younger than 30 days, daily
    otherwise. Errors on unknown markets and on markets that cannot serve
    price history. *)
val get_market_detail : (Slug.t, Market_detail.t Or_error.t) Rpc.Rpc.t

(** The pairs currently in the given status, in review-listing order. *)
val get_pairs : (Pair_status.t, Pair_card.t list Or_error.t) Rpc.Rpc.t

(** Move one listed pair to a new status; returns it as decided. *)
val decide_pair : (Decide_request.t, Pair_card.t Or_error.t) Rpc.Rpc.t

(** Adjudicate every Proposed pair with the LLM: true twins land in Approved,
    lookalikes in Rejected, each with the model's one-line rationale. The
    human can overrule any verdict via {!decide_pair}. Costs real tokens —
    the page says so out loud. *)
val llm_review_pairs
  : (Llm_review_request.t, Llm_review_summary.t Or_error.t) Rpc.Rpc.t

(** Approve every Proposed pair scoring at least the threshold, leaving the
    rest Proposed. Free and deterministic, but only as sound as the text
    matcher whose scores it trusts — the page warns as much. Reversible per
    pair via {!decide_pair}. *)
val auto_review_pairs
  : (Auto_review_request.t, Auto_review_summary.t Or_error.t) Rpc.Rpc.t

(** Page through both venues' full open listings, text-match the cross
    product, and file the survivors for review. Slow — a minute or two of
    venue paging — and free: no LLM is involved. *)
val run_sweep : (Sweep_request.t, Sweep_summary.t Or_error.t) Rpc.Rpc.t

(** Price every approved pair against both venues' live order books once —
    one tick of the paper bot's loop, without placing orders. Tradable edges
    are booked into the wallet as a side effect. *)
val scan_edges : (unit, Scan_report.t Or_error.t) Rpc.Rpc.t

(** Can this server place real orders, and if not, why. *)
val get_execution_capability
  : (unit, Execution_capability.t Or_error.t) Rpc.Rpc.t

(** Advisory rails check for one pair before the user commits money to a
    manual leg. *)
val preflight_hedge : (Preflight_request.t, Preflight.t Or_error.t) Rpc.Rpc.t

(** Fire the Kalshi hedge for a user-confirmed manual leg — sized to the
    confirmed count, slippage-guarded, inside the rails, idempotent per
    nonce. *)
val execute_hedge : (Hedge_request.t, Hedge_result.t Or_error.t) Rpc.Rpc.t

(** The wallet as it stands. *)
val get_wallet : (unit, Wallet.t Or_error.t) Rpc.Rpc.t

(** Mark one booked pair (by [pair_key]) as really traded, freezing its
    dollars into the acted column; returns the refreshed wallet. *)
val mark_acted : (string, Wallet.t Or_error.t) Rpc.Rpc.t

(** Parse and backtest a bot program against live Kalshi history for the
    requested markets. Errors carry parse locations / validation context
    verbatim for display on the rules page. *)
val run_simulation : (Sim_request.t, Sim_result.t Or_error.t) Rpc.Rpc.t

(** Where the trading key stands: connected? unlocked? demo or production? *)
val get_trading_key : (unit, Trading_key.Status.t Or_error.t) Rpc.Rpc.t

(** Validate, encrypt, and store a pasted key, replacing any previous one —
    then unlock it if the server allows live trading. The only RPC whose
    request carries secrets. *)
val connect_trading_key
  : ( Trading_key.Connect_request.t
      , Trading_key.Status.t Or_error.t )
      Rpc.Rpc.t

(** Decrypt the stored key with the passphrase (the query) and install the
    live executor. Refused on a server started without [-allow-live], or with
    a wrong passphrase. *)
val unlock_trading_key : (string, Trading_key.Status.t Or_error.t) Rpc.Rpc.t

(** Drop the decrypted credentials and the live executor from server memory.
    The encrypted file stays; {!unlock_trading_key} brings it back. *)
val lock_trading_key : (unit, Trading_key.Status.t Or_error.t) Rpc.Rpc.t

(** Lock, then delete the encrypted key file — full disconnect. *)
val forget_trading_key : (unit, Trading_key.Status.t Or_error.t) Rpc.Rpc.t

(** The unlocked key's cash and open positions, straight from the venue.
    Errors when no key is unlocked. *)
val get_account : (unit, Account.t Or_error.t) Rpc.Rpc.t

(** Engage the kill switch: creates the sentinel file with the given reason
    so every live order refuses from now on. One-way from the browser —
    clearing it requires deleting the file on the server machine. Returns the
    engaged reason as confirmation. *)
val trip_kill_switch : (string, string Or_error.t) Rpc.Rpc.t
