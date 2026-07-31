open! Core
open Types

type t =
  { order : Order.t
  ; filled_size : Size.t
  ; price : Price.t
  ; fee : Price.t
  ; venue_order_id : string option
  }
[@@deriving sexp_of]

let cost t =
  let gross = Size.multiply_by_price t.filled_size t.price in
  match t.order.side with
  | Buy -> Price.( + ) gross t.fee
  | Sell -> Price.( - ) t.fee gross
;;
