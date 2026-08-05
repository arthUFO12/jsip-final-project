open! Core
open! Types

(** Answers "are these two markets the same bet?" without seeing a price.

    Two passes. First each stub is compiled to a typed {!Claim.t}; when both
    sides of a pair compile, {!Claim.classify}'s relation is final — kept iff
    [Equivalent], and a wrong entity, style, or bound is a hard veto no
    string score can overrule. Only pairs with at least one [Opaque] side
    fall back to the string pipeline: narrow the candidate set using tags,
    score survivors by string similarity (trigram Jaccard), veto candidates
    whose numbers conflict. Callers only see the combined pipeline via
    {!find_candidates}. Never touches the network, never makes a trade
    decision. Fully testable with hand-written examples. *)

module Candidate : sig
  type t =
    { left : Market_stub.t
    ; right : Market_stub.t
    }
  [@@deriving sexp_of]
end

(** [find_candidates ~threshold ~apply_veto lefts rights] returns the pairs
    that block together (by shared title tag, or by shared
    {!Claim.Blocking_key} when both sides compile) and survive the gates. A
    pair whose two claims both compile is decided by {!Claim.classify} alone
    — kept iff [Equivalent], with [threshold] and [apply_veto] ignored. A
    pair with an [Opaque] side must have title similarity at least
    [threshold] (0.0 to 1.0) and — when [apply_veto] is true — survive the
    numeric veto. Callers should take both settings from {!Config.Matching}:
    a loose threshold with the veto off when an LLM adjudicates afterwards,
    strict with the veto on when this pipeline has the final word. *)
val find_candidates
  :  threshold:float
  -> apply_veto:bool
  -> Market_stub.t list
  -> Market_stub.t list
  -> Candidate.t list

module Comparison : sig
  (** One pair as judged by both matching systems independently, for
      side-by-side evaluation of the claim gate against the string pipeline.
      {!find_candidates} is defined as the tradeable subset of this report
      (every bucket except [Conflict]), so the comparison can never drift
      from what the pipeline actually proposes. *)

  module Bucket : sig
    type t =
      | Both (** claims say Equivalent and the string pipeline passes *)
      | Claims_only
      (** claims say Equivalent; the string pipeline missed it — too low a
          similarity score, or no shared title tag at all *)
      | Text_only
      (** the string pipeline passes; claims abstained because at least one
          side is [Opaque] *)
      | Conflict of Claim.Relation.t
      (** the string pipeline passes but the claims veto: either one of the
          old system's false positives, or a claim parser bug — exactly the
          pairs worth a human look *)
      | Neither
      (** blocked together but rejected by both systems. The near-misses:
          reviewing the best-scoring of these answers "is either matcher
          missing real pairs?" *)
    [@@deriving sexp_of]
  end

  type t =
    { candidate : Candidate.t
    ; bucket : Bucket.t
    ; score : float (** trigram similarity, whatever the bucket *)
    ; veto : string option
    (** why the numeric veto fired, if it did — so a rejected pair's report
        row can say what the accept gate actually read *)
    ; deciding : string
    (** the claims' reason: which field decided ({!Claim.Verdict}), which
        field abstained, or ["unparsed"] *)
    ; left_claim : Claim.t option (** [None] = did not compile *)
    ; right_claim : Claim.t option
    }
  [@@deriving sexp_of]

  module Coverage : sig
    (** Per-domain accounting over every blocked pair: how many the claims
        decided vs abstained on, with each abstention's cause counted —
        "claims only speak rates" as a table, not an inference. Keyed by
        {!Claim.Domain.to_string} (or ["(unparsed)"] when neither side
        compiled). *)
    module Row : sig
      type t =
        { pairs : int
        ; decided : int
        ; abstentions : int String.Map.t (** cause -> count *)
        }
      [@@deriving sexp_of]

      val empty : t
      val merge : t -> t -> t
    end

    type t = Row.t String.Map.t [@@deriving sexp_of]

    val merge : t -> t -> t
  end

  module Report : sig
    type comparison := t

    (** Millions of blocked pairs are judged, but almost all are [Neither]
        junk collisions: those exist here only as [neither] counts and the
        bounded best-scoring [near_misses] sample, never as a list — at
        full-listing scale that list alone is gigabytes. *)
    type t =
      { judged : comparison list (** every non-[Neither] pair *)
      ; near_misses : comparison list
      (** best-scoring [Neither]s, descending *)
      ; neither : int
      ; coverage : Coverage.t
      }
    [@@deriving sexp_of]

    val empty : t
  end
end

(** [compare_pipelines ~threshold ~apply_veto lefts rights] streams every
    blocked pair through both systems and returns the report. Pure and
    LLM-free — [threshold] and [apply_veto] configure only the string side. *)
val compare_pipelines
  :  threshold:float
  -> apply_veto:bool
  -> Market_stub.t list
  -> Market_stub.t list
  -> Comparison.Report.t

(** Internals of the pipeline, exposed so tests can exercise each phase
    directly. Production code should only use {!find_candidates}. *)
module For_testing : sig
  val normalize : string -> string
  val trigrams : string -> String.Set.t
  val jaccard : string -> string -> float
  val score : Candidate.t -> float
  val block : Market_stub.t list -> Market_stub.t list -> Candidate.t list
  val veto : Candidate.t -> string option
end

(** [score candidate] is the trigram-jaccard similarity of the two titles
    after normalization, 0.0 to 1.0. The number stored on a
    {!Pair_proposal.t} so review can rank proposals. *)
val score : Candidate.t -> float

(** The blocking tags of a title: its normalized words, minus stopwords and
    single characters. Blocking compares only markets sharing a tag; the
    sweep reuses these words as its search query. *)
val tags : string -> String.Set.t
