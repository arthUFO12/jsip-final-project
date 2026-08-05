(** Market signals — the leaves of a rule's boolean expression that look at
    price history rather than at variables. A signal corresponds to the
    spec's [SLUG (UP|DOWN) BY <amount|pct>] boolean statements, plus simple
    threshold checks. The surface syntax always tracks the yes price
    ({!Parse} builds [Moved] with [contract = Yes]); the [contract] field
    stays because threshold signals may still watch either side.

    Signals are pure data; {!evaluate} reads prices through
    {!Eval_env.price_ago}. When history is too short to answer (either window
    endpoint is unknown) a signal evaluates to [false] — note this makes
    [!signal] vacuously true on short history. *)

open! Core
open Types

module Direction : sig
  type t =
    | Up
    | Down
  [@@deriving sexp_of, compare, equal, hash]
end

module Magnitude : sig
  type t =
    | Amount of float (** Absolute price change, in dollars. *)
    | Percent of float
    (** Change relative to the start-of-window price; [Percent 5.] means 5%.
        A window starting at price zero evaluates to [false]. *)
  [@@deriving sexp_of, compare, equal, hash]
end

module Window : sig
  (** The lookback window a change is measured over, as offsets back from the
      current tick. [start_ago] is the earlier endpoint, so a valid window
      has [start_ago >= end_ago]. *)
  type t =
    { start_ago : Time_ns.Span.t
    ; end_ago : Time_ns.Span.t (** [Time_ns.Span.zero] = the current tick. *)
    }
  [@@deriving sexp_of, compare, equal, hash]

  (** [since span] measures from [span] ago up to the current tick. *)
  val since : Time_ns.Span.t -> t
end

type t =
  | Moved of
      { slug : Slug.t
      ; contract : Contract_type.t
      ; direction : Direction.t
      ; window : Window.t
      ; by : Magnitude.t
      (** Fires when moved at least this much ([>=], with a tolerance far
          below price resolution so exact boundaries fire despite float
          rounding — $0.40 to $0.50 counts as "up by 0.10"). *)
      }
  | Above of
      { slug : Slug.t
      ; contract : Contract_type.t
      ; threshold : float (** Dollars; fires when price > threshold. *)
      }
  | Below of
      { slug : Slug.t
      ; contract : Contract_type.t
      ; threshold : float (** Dollars; fires when price < threshold. *)
      }
[@@deriving sexp_of, compare, equal, hash]

include Hashable.S_plain with type t := t

(** Every slug the signal reads, for {!Program} validation. *)
val slug : t -> Slug.t

(** The signal in the program syntax that would parse back to it, with the
    ticker in its {!Ticker_name.normalize}d spelling — e.g.
    [kxelonmars_99 up by 5% since 1d ago end 2h ago]. Used for the "why this
    rule fired" reasons attached to simulated trades. The contract is not
    printed: {!Parse} only builds yes-side signals. *)
val to_string : t -> string

val evaluate : t -> env:Eval_env.t -> bool
