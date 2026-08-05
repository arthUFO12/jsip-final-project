open! Core
open! Types

(** Answers "is there edge on this pair right now?" using prices only.

    Given the two legs of a confirmed pair — each carrying its venue, market
    id, and current book — prices both ways of splitting the trade (YES on
    one leg, NO on the other), at asks including each venue's taker fee, and
    reports whichever is profitable along with how much size is actually
    available. Returns nothing if there's no edge. Venue-agnostic: any two
    venues in {!Venue.t} can face each other, and adding a venue only
    requires naming its fee. Sees no titles. No I/O, no clock, no randomness
    — same inputs always give the same answer. *)

module Leg : sig
  (** One side of a confirmed pair: which market on which venue, and what its
      book offers right now. The caller (bot layer) builds these from live
      feeds ({!Binary_book} comes from the venue's order-book endpoint); the
      pair store described in [lib/types/src/pairs.mli] will supply the venue
      and market id once the database layer lands. *)
  type t =
    { venue : Venue.t
    ; market_id : Market_id.t
    ; book : Binary_book.t
    }
  [@@deriving sexp_of]
end

module Entry : sig
  (** One executable order implied by an opportunity: buy this outcome's leg
      on this venue and market at up to [price] — the ask that was quoted
      when the opportunity was detected, and so the natural limit price for
      the order that acts on it. A {!Leg.t} minus the book. *)
  type t =
    { venue : Venue.t
    ; market_id : Market_id.t
    ; price : Price.t
    }
  [@@deriving sexp_of]
end

module Mode : sig
  (** How honestly the detector prices an opportunity.

      [Exact] is the truth: asks plus each venue's taker fee, and an
      opportunity only exists if both books have size behind it. This is the
      only mode that should ever drive live orders.

      [Reckless] ignores fees and ignores whether any contracts are actually
      available: every [cost < $1] pair of asks is reported, sized or not.
      The reported [edge] overstates reality by both venues' fees, and [size]
      may be zero. Deliberately dangerous — it exists to surface near-misses
      in paper mode (what would we see if fees were lower?), never to trade.
      Wire it behind config so choosing it is loud. *)
  type t =
    | Exact
    | Reckless
  [@@deriving sexp_of]
end

type opportunity =
  { yes : Entry.t (** where to buy YES *)
  ; no : Entry.t (** where to buy NO *)
  ; cost : Price.t (** combined entry, asks + fees *)
  ; edge : Price.t (** [Price.one - cost] *)
  ; size : Size.t (** min depth across both legs *)
  }
[@@deriving sexp_of]

(** [find ~mode left right] prices both splits of the pair at current asks
    and returns the profitable one — the better one, if both clear — or
    [None] when neither does. Under {!Mode.Exact} fees are included and
    sizeless edges are dropped; under {!Mode.Reckless} neither is true, and
    the result must not be traded. [mode] is required so every caller states
    which detector it wants. The two legs are interchangeable:
    [find left right] and [find right left] agree. *)
val find : mode:Mode.t -> Leg.t -> Leg.t -> opportunity option

(** [cost_and_size ~mode ~yes ~no] is the combined entry cost (asks, plus
    fees under {!Mode.Exact}) and available size of one split: buy YES on
    [yes], NO on [no]. Unlike {!find} it reports unprofitable splits too — a
    cost of [$1] or more just means no edge — which is what a display surface
    needs to show {e why} nothing is tradable. Anything that acts on prices
    should use {!find}. *)
val cost_and_size : mode:Mode.t -> yes:Leg.t -> no:Leg.t -> Price.t * Size.t

(** The pure pieces of the pipeline, exposed so tests can exercise them
    directly. Production code should only use {!find}. *)
module For_testing : sig
  (** Kalshi's taker fee for one contract bought at this price:
      [ceil (0.07 * P * (1 - P))], rounded up to a whole cent. Delegates to
      {!Execution.Fees}, which the executors also charge — one source of fee
      truth. *)
  val kalshi_fee : Price.t -> Price.t

  (** The taker fee one venue charges for one contract at this price. *)
  val taker_fee : Venue.t -> Price.t -> Price.t

  (** Combined entry cost (asks, plus fees under {!Mode.Exact}) and available
      size for one split. *)
  val cost_and_size
    :  mode:Mode.t
    -> yes:Leg.t
    -> no:Leg.t
    -> Price.t * Size.t
end
