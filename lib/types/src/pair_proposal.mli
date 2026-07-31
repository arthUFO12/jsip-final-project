open! Core

(** One proposed market pair moving through the review gate: matched by text
    (and optionally adjudicated by the LLM), waiting for a human to approve
    or reject, then served to the bot if approved. The two sides are market
    ids; titles and venues live in the market-stub store. A rejected pair
    stays rejected — re-proposing it must not resurrect it.

    See [pairs.mli] for how the store fits the pipeline; persistence lives in
    the database library. *)

module Status : sig
  type t =
    | Proposed
    | Approved
    | Rejected
  [@@deriving sexp_of, equal, string]
end

type t =
  { left : Market_id.t
  ; right : Market_id.t
  ; score : float (** the {!Matcher} similarity that proposed it *)
  ; explanation : string option
  (** the LLM's reasoning, when it adjudicated *)
  ; status : Status.t
  }
[@@deriving sexp_of]

(** [create ~left ~right ~score ~explanation] is a [Proposed] pair with the
    sides in canonical order, so proposing (A, B) and (B, A) yields the same
    pair. *)
val create
  :  left:Market_id.t
  -> right:Market_id.t
  -> score:float
  -> explanation:string option
  -> t
