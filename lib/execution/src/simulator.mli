open! Core
open! Async
open Types

(** Pretends to trade. Fills orders under a chosen fill model, owns the open
    positions and the running cash balance, and reports each fill in the same
    {!Fill.t} shape the live path uses. The fill model is the dial you turn
    to show how much of a paper edge survives contact with reality:
    [At_limit] is the optimist, [Against_live_book] pays real asks, real
    depth limits, and real fees.

    The simulator only models taker fills — an order that would rest on the
    book (limit below the ask) is rejected, not queued. It also refuses to
    short: a sell larger than the held position is an error, matching what a
    cash account at the venue would do. *)

module Fill_model : sig
  type t =
    | At_limit
    (** fill the whole order at its limit price: no depth limit, no fees. The
        most generous model — anything unprofitable here is unprofitable
        everywhere. *)
    | Against_live_book
    (** fetch the market's live book, cross at the ask, clamp to the depth
        actually there, and charge the venue's taker fee. Buys only —
        {!Binary_book.t} carries no bid depth to sell into. *)
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val create : fill_model:Fill_model.t -> starting_cash:Price.t -> t

(** Cash remaining: starting cash minus every {!Fill.cost} so far. *)
val cash : t -> Price.t

(** Contracts held right now; {!Size.zero} for a market never traded. *)
val position
  :  t
  -> market_id:Market_id.t
  -> contract:Contract_type.t
  -> Size.t

(** [place_order t order] fills [order] under the fill model and updates cash
    and positions, or explains why not: limit below the ask, no depth, not
    enough cash, selling more than held. Only [Against_live_book] does I/O
    (one book fetch). *)
val place_order : t -> Order.t -> Fill.t Deferred.Or_error.t

module For_testing : sig
  (** The pure core of [Against_live_book], with the book supplied instead of
      fetched. *)
  val fill_against_book
    :  t
    -> Order.t
    -> book:Binary_book.t
    -> Fill.t Or_error.t
end
