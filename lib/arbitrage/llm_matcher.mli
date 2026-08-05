open! Core
open! Async
open! Types

(** Optional second opinion on candidates the string matcher found ambiguous.
    Sends a pair to Claude over the Anthropic Messages API and gets back
    yes/no plus a short explanation of which attributes drove the decision.
    Caches every verdict so the same pair is never asked twice and reruns are
    deterministic. Can be switched off entirely (see {!Config.Matching})
    without affecting anything else. Its output is advisory: it feeds the
    review step, never a trade. *)

type verdict =
  { is_match : bool (** true when both markets settle on the same event *)
  ; explanation : string (** one or two sentences on what drove the call *)
  }
[@@deriving sexp_of]

(** [adjudicate ?api_key candidate] returns the cached verdict for this pair
    if one exists, otherwise asks the model and caches the answer. [api_key]
    overrides the [ANTHROPIC_API_KEY] environment variable — how a web user
    brings their own key; it is used for the call and never stored. Errors
    (missing API key, network failure, malformed response) are returned,
    never raised, so one bad pair can't take down a matching sweep. *)
val adjudicate
  :  ?api_key:string
  -> Matcher.Candidate.t
  -> verdict Or_error.t Deferred.t

(** The pure pieces of the pipeline, exposed so tests can exercise them
    without touching the network. *)
module For_testing : sig
  val cache_key : Matcher.Candidate.t -> Market_id.t * Market_id.t
  val request_body : Matcher.Candidate.t -> Yojson.Safe.t
  val parse_verdict : string -> verdict Or_error.t

  (** The in-memory verdict cache, exposed so tests can seed and clear it. *)
  val cache : (Market_id.t * Market_id.t, verdict) Hashtbl.t
end
