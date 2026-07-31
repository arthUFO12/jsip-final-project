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
  ; series_ticker : Slug.t option
      (* Kalshi only: the recurring series an event belongs to EX:
         KXELONMARS. [None] for Polymarket. *)
  ; clob_token_id : string option (* polymarket specific *)
  ; category : Category.t
      (* Topmost classification EX: Crypto, Politics, Economics;
         [Miscellaneous] when the venue's label maps to nothing we track *)
  ; yes_bid : Price.t option (* Highest price someone is buying *)
  ; yes_ask : Price.t option (* Lowest price someone is selling *)
  ; last_price : Price.t option (* Most recent sale price *)
  ; volume : Volume.t option (* Lifetime traded volume *)
  ; volume_24h : Volume.t option (* Traded volume, trailing 24 hours *)
  ; active : bool
  ; created_time : Time_ns.t
  ; close_time : Time_ns.t
  }
[@@deriving sexp_of, fields]

let to_market_stub t : Market_stub.t =
  { venue = t.venue
  ; market_id = t.market_id
  ; slug = t.slug
  ; series_ticker = t.series_ticker
  ; clob_token_id = t.clob_token_id
  ; title = t.title
  ; created_time = t.created_time
  ; close_time = t.close_time
  ; category = t.category
  ; volume = t.volume
  }
;;
