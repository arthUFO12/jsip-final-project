open! Core
open! Types

(** One record holding every knob the user can turn: trading mode, matching
    behavior, execution parameters. Parsed from a file or flags and validated
    once at startup via {!validate}. Everything downstream receives config as
    data — nothing reads it from a global. *)

module Matching : sig
  (** How proposed market pairs get confirmed. The string pipeline in
      {!Matcher} always runs first; this knob decides how much it is trusted.

      In [Llm_assisted] mode the pipeline is only a funnel: [threshold] is
      set low to shed obvious junk, the numeric/date veto is off (a false
      veto would hide the pair from the LLM forever), and every surviving
      candidate goes to {!Llm_matcher} for the real decision.

      In [Text_only] mode the pipeline is the final arbiter: [threshold] is
      high and the veto is on, because nothing runs after it. *)
  type t =
    | Llm_assisted of { threshold : float }
    | Text_only of { threshold : float }
  [@@deriving sexp_of]

  (** Sensible defaults for each mode: a loose 0.1 funnel for [Llm_assisted],
      a strict 0.5 cutoff for [Text_only]. *)
  val default_llm_assisted : t

  val default_text_only : t

  (*_ Accessors used to drive {!Matcher.find_candidates}. *)
  val threshold : t -> float
  val apply_veto : t -> bool
  val use_llm : t -> bool
end

module Trading : sig
  (** What happens when the bot finds an opportunity. [Paper] fills the
      orders the opportunity implies in an {!Execution.Simulator}; [Live]
      sends them to the venues through {!Execution.Kalshi_live}, which needs
      the credentials named in {!Execution.Kalshi_live.Credentials} in the
      environment. *)
  type t =
    | Paper
    | Live
  [@@deriving sexp_of]

  (** [Paper] — going live must always be an explicit choice. *)
  val default : t
end

module Execution : sig
  (** What the bot loop reads each tick. *)
  type t =
    { poll_interval : Time_ns.Span.t (** how often to refresh books *)
    ; stake_per_opportunity : Size.t
    (** max contracts to take per hit; caps {!Detect.opportunity}'s [size] *)
    ; min_edge : Price.t
    (** ignore edges thinner than this — a 1-microcent edge is real to
        {!Detect.find} but not worth crossing two spreads for *)
    ; detect_mode : Detect.Mode.t
    (** [Exact] or [Reckless]; see {!Detect.Mode} for why [Reckless] must
        never drive real orders *)
    ; max_dollars_per_order : Price.t
    (** hard cap on one live order's notional (limit x size); checked before
        every live placement *)
    ; max_dollars_per_day : Price.t
    (** hard cap on live notional per UTC day, summed from the trade log; an
        order that would cross it is refused *)
    }
  [@@deriving sexp_of]

  (** 1s polls, 10 contracts, 1c minimum edge, [Exact] pricing, $25 per order
      and $100 per day — deliberately small; raising the caps is an explicit
      act. *)
  val default : t
end

type t =
  { matching : Matching.t
  ; trading : Trading.t
  ; execution : Execution.t
  }
[@@deriving sexp_of]

(** {!Matching.default_llm_assisted}, {!Trading.default},
    {!Execution.default}. Valid by construction: [validate default] always
    succeeds. *)
val default : t

(** [validate t] returns [t] unchanged if every knob is sane, or one error
    naming all the offending knobs at once: non-positive [poll_interval],
    zero [stake_per_opportunity], negative [min_edge], [Live] trading
    combined with [Reckless] detection (reckless pricing may only ever feed
    paper trading), and [Live] trading with a non-positive spending cap —
    real money must run inside hard caps. Call it once at startup, before
    anything reads the config. *)
val validate : t -> t Or_error.t
