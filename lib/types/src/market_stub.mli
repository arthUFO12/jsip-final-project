open! Core

type t =
  { venue : Venue.t
  ; market_id : Market_id.t
  ; slug : Slug.t
  ; series_ticker : Slug.t option
  ; clob_token_id : string option
  ; title : string
  ; category : Category.t
  ; created_time : Time_ns.t
  ; close_time : Time_ns.t
  ; volume : Volume.t option
  (** Lifetime traded volume; [None] when the venue did not report one. *)
  }
[@@deriving sexp]

(** The venue can serve this market's price history: Kalshi candlesticks need
    [series_ticker], Polymarket's prices-history needs [clob_token_id].
    Polymarket serves prices only — no per-candle volume. *)
val can_fetch_history : t -> bool

val to_string : t -> string
