open! Core
open Types

(** One fully-specified order: which market, which outcome, buy or sell, at
    what limit, for how many contracts. This is the type every executor —
    {!Simulator} or {!Kalshi_live} — accepts, so a strategy builds orders
    without knowing whether they will be pretended or sent. The market is a
    whole {!Market_stub.t} rather than a bare id because carrying it out
    needs venue-specific routing data (Kalshi's ticker, Polymarket's
    [clob_token_id]). *)
type t =
  { market : Market_stub.t
  ; contract : Contract_type.t
  ; side : Side.t
  ; limit_price : Price.t
  (** per contract; venues quote whole cents, and live routing rejects
      anything finer *)
  ; size : Size.t
  }
[@@deriving sexp_of]

val venue : t -> Venue.t
