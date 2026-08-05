open! Core
open! Async
open! Types

(** The human side of the pair gate: list what {!Sweep} proposed, approve or
    reject one entry. This is the single implementation behind every review
    surface — the CLI's [review] verb today, a server RPC tomorrow — so the
    numbering a user approves against is always the numbering they were
    shown. Reads and writes go through the {!Database} store;
    {!Database.Database_exec.init_database} must have run first. *)

module Listed : sig
  (** One proposal joined back to its market stubs, tagged with its position
      in the listing. [index] is what a human quotes back to {!decide}; it is
      stable across calls because the store lists proposals in insertion
      order. A [None] stub means the market vanished from the store after the
      proposal was filed — display it, but expect {!Bot} to skip it. *)
  type t =
    { index : int
    ; proposal : Pair_proposal.t
    ; left : Market_stub.t option (** stub for [proposal.left], if stored *)
    ; right : Market_stub.t option
    (** stub for [proposal.right], if stored *)
    }
  [@@deriving sexp_of]

  (** Human-readable rendering, e.g.
      {[
        " 3. [0.82] Fed cut in March? [Kalshi]  <->  Fed cuts rates March \
         [Polymarket]\n\
        \       llm: same settlement event"
      ]} *)
  val to_display_string : t -> string
end

(** The proposals currently in [status], in listing order. Only [Proposed]
    entries can be decided; [Approved] is what {!Bot} trades, [Rejected] is
    kept so a sweep never re-adjudicates a dismissed pair. *)
val list_by_status
  :  Pair_proposal.Status.t
  -> Listed.t list Deferred.Or_error.t

(** [list_by_status Proposed] — the queue a reviewer works through, and the
    numbering {!decide} accepts. *)
val list_proposed : unit -> Listed.t list Deferred.Or_error.t

(** [decide ~from ~index ~status] moves the [index]th proposal of the [from]
    listing to [status] and returns it as decided. [from] other than
    [Proposed] is the undo path: pull a pair back out of [Approved] or
    [Rejected] when a verdict — human or LLM — was wrong. Errors if [index]
    is out of range of that listing. *)
val decide
  :  from:Pair_proposal.Status.t
  -> index:int
  -> status:Pair_proposal.Status.t
  -> Listed.t Deferred.Or_error.t

module Llm_review_summary : sig
  (** One LLM pass over the Proposed queue: [reviewed] entries read,
      [approved]/[rejected] verdicts filed (rationale stored as the pair's
      explanation), [errored] pairs left untouched — with the first error
      verbatim, since one bad API key errors the whole batch the same way. *)
  type t =
    { reviewed : int
    ; approved : int
    ; rejected : int
    ; errored : int
    ; first_error : string option
    }
  [@@deriving sexp_of]
end

module Auto_review_summary : sig
  (** One deterministic pass over the Proposed queue: [reviewed] entries
      read, [approved] at or above the threshold, [left_proposed] below it —
      untouched, for a human or the LLM. *)
  type t =
    { reviewed : int
    ; approved : int
    ; left_proposed : int
    }
  [@@deriving sexp_of]
end

(** [auto_review_proposed ~threshold] approves every Proposed pair whose
    stored text-match score is at least [threshold], stamping the rule that
    did it as the pair's explanation; pairs below stay Proposed. No model and
    no reading of titles — this trusts the text matcher that filed the pairs
    completely, so its verdicts are only as sound as those scores. Reversible
    per pair via {!decide}. *)
val auto_review_proposed
  :  threshold:float
  -> Auto_review_summary.t Deferred.Or_error.t

(** [llm_review_proposed ?api_key ()] adjudicates every Proposed pair with
    {!Llm_matcher}: true twins land in [Approved], lookalikes in [Rejected],
    each carrying the model's one-line rationale. Pairs whose calls fail are
    left [Proposed] for a retry. The human remains the court of appeal via
    {!decide}. [api_key] overrides the server's [ANTHROPIC_API_KEY]. *)
val llm_review_proposed
  :  ?api_key:string
  -> unit
  -> Llm_review_summary.t Deferred.Or_error.t
