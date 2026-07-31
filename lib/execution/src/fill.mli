open! Core
open Types

(** What an executor reports back for one {!Order.t}. The same shape comes
    from both executors so downstream accounting (PnL, position tracking)
    cannot tell simulation from reality — except by [venue_order_id], which
    is the honest marker: [None] exactly when the fill was simulated.

    A live limit order may rest instead of trading; that reports
    [filled_size = zero] with the venue's order id, and reconciling later
    fills is future work (the venue's fills endpoint). *)
type t =
  { order : Order.t
  ; filled_size : Size.t (** contracts known filled right now *)
  ; price : Price.t
  (** per contract actually paid (or received, on a sell) *)
  ; fee : Price.t (** total venue fee for [filled_size] contracts *)
  ; venue_order_id : string option (** [None] exactly when simulated *)
  }
[@@deriving sexp_of]

(** [cost t] is the cash this fill consumed: [price * filled_size + fee] on a
    buy; [fee - price * filled_size] on a sell, so a profitable sell is
    negative (cash came in). *)
val cost : t -> Price.t
