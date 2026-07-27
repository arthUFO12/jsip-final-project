(** Decodes venue price-history payloads into {!Time_series_point.t} samples.
    Bodies come from {!Fetch_time_series}; both functions raise on malformed
    input.

    - Kalshi: candlestick shape [{ "candlesticks": [...] }]; the yes price is
      the bid/ask close midpoint.
    - Polymarket: [{ "history": [{"t": .., "p": ..}, ...] }] from clob
      /prices-history; [p] is the yes price directly. *)

open! Core

val parse_kalshi_time_series : string -> Time_series.Point.t list
val parse_polymarket_time_series : string -> Time_series.Point.t list
