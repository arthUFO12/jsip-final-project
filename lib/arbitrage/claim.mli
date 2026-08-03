open! Core
open! Types

(** One record, six fields: what a market settles on, compiled from its stub
    so matching compares typed claims instead of strings.

    {!verdict} compares two claims field by field ({!Field_cmp}) and
    aggregates: a hard disagreement on any identity field decides the pair;
    an [Unknown] abstains — on that one pair, on that one field — handing it
    to the string pipeline instead of silently passing or failing a whole
    domain. Every false-positive class in the comparison reports maps to one
    field here: wrong player = [subject], EPL-vs-UCL = [arena], map 1 vs game
    2 = [scope], 2026-vs-2027 = [period], Winner-vs-O/U or #1-vs-#2 =
    [outcome]. *)

module Field_cmp : sig
  type t =
    | Agree
    | Disagree (** both sides confident, and different *)
    | Unknown (** at least one side unresolved: abstain, don't guess *)
  [@@deriving sexp_of, compare, equal]
end

module Domain : sig
  type t =
    | Rates
    | Championship
    | Award
    | Statistic
    | Fixture (** one match/game: "A vs B" *)
    | Media_rank
  [@@deriving sexp_of, compare, equal, enumerate]

  val to_string : t -> string
end

module Entity : sig
  (** Normalized name with an explicit resolution status. Resolution runs
      through an alias table (seeded from observed cross-venue collision
      classes; extend it as new ones appear) after lowercasing, folding
      diacritics (Dembélé = Dembele), and dropping filler words. *)
  type t =
    { key : string (** normalized; canonical when [resolved] *)
    ; resolved : bool
    }
  [@@deriving sexp_of, compare, equal]

  val of_raw : string -> t

  (** Identical keys agree regardless of resolution; different keys are a
      confident [Disagree] only when both sides resolved through the table,
      and [Unknown] otherwise — an unrecognized name must abstain, never
      manufacture a verdict. *)
  val cmp : t -> t -> Field_cmp.t

  (*_ Exposed so the rate parser can share the bank spellings. *)
  val aliases : (string * string list) list
  val normalize : string -> string
  val fold_diacritics : string -> string
end

module Bound : sig
  (** A constraint on a number. Signed values make hike/cut arithmetic, not
      lexical: a 25 bps hike is [Exactly 25.], a more-than-25 bps cut is
      [Less_than (-25.)]. *)
  type t =
    | Exactly of float
    | At_least of float
    | At_most of float
    | Greater_than of float
    | Less_than of float
    | Between of float * float (** inclusive *)
  [@@deriving sexp_of, compare, equal]

  val to_interval : t -> float * float
  val overlaps : t -> t -> bool
  val equivalent : t -> t -> bool

  (** The two bounds partition the line: exactly one of the two markets
      settles YES, so buying YES on both locks in $1. *)
  val complementary : t -> t -> bool

  (** [subset a b]: [a] settling YES entails [b] settling YES. *)
  val subset : t -> t -> bool
end

module Style : sig
  (** How a threshold resolves: value as of the deadline, or any time at or
      before it. Different styles are different instruments. *)
  type t =
    | Terminal
    | Touch
  [@@deriving sexp_of, compare, equal]
end

module Outcome : sig
  type t =
    | Win
    | Place of int (** finishing position; [Place 1] equals [Win] *)
    | Rate_move of Bound.t (** signed bps: +25 hike, -25 cut, 0 hold *)
    | Threshold of
        { bound : Bound.t
        ; style : Style.t
        }
  [@@deriving sexp_of, compare, equal]
end

module Scope : sig
  (** Which part of the event settles the market: the whole thing, or one leg
      ("map 1", "set 2"). A winner market and a map-1 market are different
      instruments even with everything else equal. *)
  type t =
    | Whole_event
    | Leg of
        { kind : string (** normalized: "game" covers map/game *)
        ; index : int
        }
  [@@deriving sexp_of, compare, equal]

  val cmp : t -> t -> Field_cmp.t
end

module Interval : sig
  (** Half-open absolute time window. "before Jan 1, 2027" and "in 2026"
      share [hi = 2027-01-01) and agree; nested windows imply, they never
      equate. *)
  type t =
    { lo : Date.t option (** [None] = open start ("before X", "by X") *)
    ; hi : Date.t (** exclusive *)
    }
  [@@deriving sexp_of, compare, equal]

  val of_year : int -> t
  val of_month : year:int -> month:Month.t -> t
  val cmp : t -> t -> Field_cmp.t

  (** [nested a b]: [a] sits inside [b] — for cumulative events, a-YES
      entails b-YES. *)
  val nested : t -> t -> bool
end

type t =
  { domain : Domain.t
  ; subject : Entity.t (** Rodri, the Fed, a fixture pair, a show *)
  ; arena : Entity.t option (** EPL, Ballon d'Or, a chart *)
  ; outcome : Outcome.t
  ; scope : Scope.t
  ; period : Interval.t option (** [None]: title names no window *)
  }
[@@deriving sexp_of, compare, equal]

module Relation : sig
  type t =
    | Equivalent (** same event: arb is YES @ A + NO @ B *)
    | Complementary (** exact negation: arb is YES @ A + YES @ B *)
    | Implies
    (** left YES entails right YES: a one-sided [P(A) <= P(B)] constraint,
        not a two-leg arb *)
    | Implied_by (** the mirror of [Implies] *)
    | Disjoint (** mutually exclusive, not exhaustive: no 2-leg arb *)
    | Overlapping (** both can settle YES: unsafe, never trade *)
    | Unrelated
  [@@deriving sexp_of, compare, equal]

  val tradeable : t -> bool
end

module Verdict : sig
  type t =
    | Decided of
        { relation : Relation.t
        ; deciding : string
        (** which field decided, e.g. ["arena disagrees"], so every report
            row can say why *)
        }
    | Abstained of
        { field : string
        (** the [Unknown] field: this pair goes back to the string pipeline,
            and the coverage table counts the cause *)
        }
  [@@deriving sexp_of]
end

(** [verdict a b] compares field by field and aggregates. Disagreements
    decide before Unknowns abstain, so a confident mismatch (Winner vs O/U)
    kills a pair even when another field is unresolved. *)
val verdict : t -> t -> Verdict.t

module Blocking_key : sig
  (** Coarse bucket for pairing compiled claims without walking the full
      cross product: subject key, window-end month, domain. *)
  type t =
    { subject : string
    ; period : string
    ; domain : string
    }
  [@@deriving sexp_of, compare]

  include Comparable.S_plain with type t := t
end

val blocking_key : t -> Blocking_key.t

(** [of_stub stub] compiles a stub's title, or [None] when it can't — an
    uncompiled market falls back to the string pipeline. Compound outcomes
    ("Stays with Golden State or Retires") are a parse failure, never a
    subject. Time windows come from title text where present; close metadata
    is trusted only where venues keep it accurate (crypto strike deadlines,
    fixture game days) — never for rate meetings. *)
val of_stub : Market_stub.t -> t option
