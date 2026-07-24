open! Core

type t =
  { venue : Venue.t
  ; market_id : Market_id.t
      (* primary key for trading EX: KXBTC-26JUL22-T118000 *)
  ; title : string
      (* human readable question EX: "Will BTC be above 118,000 by 5PM EDT?" *)
  ; slug : Slug.t (* URL-friendly name EX: btc-above-118k-july-22 *)
  ; event_slug : Slug.t option
      (* The parent grouping all sibling strikes share the same event *)
  ; category : Category.t option
      (* Topmost classification EX: Crypto, Politics, Economics *)
  ; yes_bid : Price.t option (* Highest price someone is buying *)
  ; yes_ask : Price.t option (* Lowest price someone is selling *)
  ; last_price : Price.t option (* Most recent sale price *)
  ; active : bool
  ; close_time : Time_ns.t option
  }
[@@deriving sexp_of, fields]
