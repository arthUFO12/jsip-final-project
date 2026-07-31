open! Core
open Types

type t =
  { cash : float
  ; realized : float
  ; unrealized : float
  ; price_ago :
      slug:Slug.t
      -> contract:Contract_type.t
      -> ago:Time_ns.Span.t
      -> float option
  }
