open! Core
open Types

type t =
  { market : Market_stub.t
  ; contract : Contract_type.t
  ; side : Side.t
  ; limit_price : Price.t
  ; size : Size.t
  }
[@@deriving sexp_of]

let venue t = t.market.venue
