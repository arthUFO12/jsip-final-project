open! Core

(** One row of the append-only live-trading audit trail: every attempt to
    move real money — placements, cancels, refusals — lands here before and
    after the venue answers, so "what did the app do with my account" is
    always answerable from one table. Paper fills are not logged; the trail
    is for real money only.

    [dollars] is the order's notional (limit x size) for placements that the
    venue accepted, and zero for refusals and cancels — it is what the
    per-day spending cap sums. Persistence lives in the database library. *)

type t =
  { at : Time_ns.t
  ; venue : string
  ; market_id : Market_id.t
  ; action : string (** ["place"] | ["cancel"] | ["status"] *)
  ; client_order_id : string option
  (** the idempotency handle sent to the venue, when the action has one *)
  ; outcome : string
  (** ["accepted"] | ["refused: <reason>"] | ["error: <detail>"] | ... *)
  ; detail : string (** human-readable order summary or venue response *)
  ; dollars : float (** notional the action committed; 0 when refused *)
  }
[@@deriving sexp_of]
