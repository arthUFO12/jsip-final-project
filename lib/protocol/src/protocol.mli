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

module Sim_request : sig
  type t =
    { slugs : Slug.t list (** 1 to 4 markets the bot trades. *)
    ; program : string (** Bot-language source, one statement per line. *)
    ; interval : Interval.t
    ; lookback_days : int (** Window is [now - lookback .. now]. *)
    ; warmup_hours : int (** History-only prefix ([sim_start_offset]). *)
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
    }
  [@@deriving sexp_of, bin_io]
end

module Sim_result : sig
  type t =
    { ticks : Tick_point.t list (** Warmup ticks included. *)
    ; fills : Fill.t list
    ; sim_start_s : float (** Warmup ends here; shade charts before it. *)
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
  (** Approve or reject the [index]th pair of the current Proposed listing —
      the same numbering {!get_pairs} just returned. *)
  type t =
    { index : int
    ; approve : bool
    }
  [@@deriving sexp_of, bin_io]
end

module Edge_leg : sig
  (** One side of a priced split: buy this pair's YES (or NO) on [venue] at
      [ask] dollars. *)
  type t =
    { venue : string
    ; title : string
    ; ask : float
    }
  [@@deriving sexp_of, bin_io]
end

module Edge_card : sig
  (** One approved pair priced against both live books: the cheaper split's
      legs, its combined [cost] (asks plus taker fees, dollars), the [edge]
      left under $1 (negative when the pair costs more than it pays), depth
      as [size] contracts, and whether the bot would act ([tradable] means
      the edge clears its minimum and has size behind it). *)
  type t =
    { yes : Edge_leg.t
    ; no : Edge_leg.t
    ; cost : float
    ; edge : float
    ; size : int
    ; tradable : bool
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

(** Current (still-open) markets from the server's database seed, in no
    particular order — grouping and ranking are client concerns. *)
val get_markets : (unit, Market_card.t list Or_error.t) Rpc.Rpc.t

(** The pairs currently in the given status, in review-listing order. *)
val get_pairs : (Pair_status.t, Pair_card.t list Or_error.t) Rpc.Rpc.t

(** Approve or reject one proposed pair; returns it as decided. *)
val decide_pair : (Decide_request.t, Pair_card.t Or_error.t) Rpc.Rpc.t

(** Page through both venues' full open listings, text-match the cross
    product, and file the survivors for review. Slow — a minute or two of
    venue paging — and free: no LLM is involved. *)
val run_sweep : (Sweep_request.t, Sweep_summary.t Or_error.t) Rpc.Rpc.t

(** Price every approved pair against both venues' live order books once —
    one tick of the paper bot's loop, without placing orders. *)
val scan_edges : (unit, Scan_report.t Or_error.t) Rpc.Rpc.t

(** Parse and backtest a bot program against live Kalshi history for the
    requested markets. Errors carry parse locations / validation context
    verbatim for display on the rules page. *)
val run_simulation : (Sim_request.t, Sim_result.t Or_error.t) Rpc.Rpc.t
