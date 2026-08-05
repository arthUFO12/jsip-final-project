(** The pure math of replaying the arbitrage strategy over two venues'
    historical price series. A replay tick has one mid price per venue —
    no order book, no depth — so the model is deliberately simple and its
    bias is one-directional: with no bid/ask spread modeled, edges here are
    {e upper bounds} on what the live scan (which pays the real ask on both
    legs) would have found. State that wherever results are shown.

    Fees are the real ones ({!Execution.Fees.taker_fee}); the strategy is
    the scan's own: price both YES/NO splits, keep the cheaper, and call
    the pair tradable when $1 minus that cost clears the threshold. *)

open! Core
open Types

module Episode : sig
  (** One taking of an edge: the strategy enters when the edge first
      clears [min_edge] and re-arms only after it drops back below — so a
      persistent edge is taken once, not once per tick. *)
  type t =
    { entered_s : float (** epoch seconds *)
    ; exited_s : float option
      (** when the edge dropped back below threshold; [None] = still open
          at the window's end *)
    ; entry_edge : float (** dollars per contract, after fees, at entry *)
    ; locked_dollars : float (** [entry_edge x stake] *)
    }
  [@@deriving sexp_of]
end

(** The after-fee edge (dollars per contract) of the cheaper split of a
    pair priced at these two YES mid prices. Negative means no arb. *)
val edge_after_fees
  :  venue_a:Venue.t
  -> yes_a:Price.t
  -> venue_b:Venue.t
  -> yes_b:Price.t
  -> float

(** Walk an edge series ([(epoch_s, edge)] in time order) with the
    enter-once / re-arm-below-threshold rule. *)
val episodes
  :  points:(float * float) list
  -> min_edge:float
  -> stake:int
  -> Episode.t list

(** Cumulative locked-in dollars as a step series at entry times, for
    charting. Empty when there are no episodes. *)
val cumulative : Episode.t list -> (float * float) list
