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

(** [decide ~index ~status] marks the [index]th listed proposal [Approved] or
    [Rejected] and returns it as decided. Errors if [index] is out of range
    of the current listing. *)
val decide
  :  index:int
  -> status:Pair_proposal.Status.t
  -> Listed.t Deferred.Or_error.t
