open! Core
open! Types

(** Answers "are these two markets the same bet?" using text only.

    Internally this runs three phases: narrow the candidate set using tags so
    you aren't comparing everything to everything; score the surviving
    candidates by string similarity (trigram Jaccard); veto any candidate
    whose numbers or dates conflict, regardless of how similar the text is.
    Callers only see the combined pipeline via {!find_candidates}. Never sees
    a price, never touches the network, never makes a trade decision. Fully
    testable with hand-written examples. *)

module Candidate : sig
  type t =
    { left : Market_stub.t
    ; right : Market_stub.t
    }
  [@@deriving sexp_of]
end

(** [find_candidates ~threshold ~apply_veto lefts rights] returns the pairs
    of markets that block together, whose title similarity is at least
    [threshold] (0.0 to 1.0), and — when [apply_veto] is true — that survive
    the numeric/date veto. Callers should take both settings from
    {!Config.Matching}: a loose threshold with the veto off when an LLM
    adjudicates afterwards, strict with the veto on when this pipeline has
    the final word. *)
val find_candidates
  :  threshold:float
  -> apply_veto:bool
  -> Market_stub.t list
  -> Market_stub.t list
  -> Candidate.t list

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
