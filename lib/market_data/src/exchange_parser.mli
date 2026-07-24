open! Core
open Types

(** Decodes a raw venue payload into normalized L1 records.

    [body] is the untouched HTTP/WS response body. Venue quirks are handled
    internally:
    - Kalshi: skips multi-leg/MVE entries if any slip through the
      [mve_filter=exclude] query.
    - Polymarket: double-decodes the [outcomePrices] escaped-string array;
      index 0 = Yes, index 1 = No.

    Returns [Error _] if [body] is not valid JSON or lacks the expected
    top-level structure. Individual malformed market entries within an
    otherwise-valid array are skipped, not fatal. *)
val parse_data
  :  body:string
  -> venue:Venue.t
  -> L1_market_metadata.t list Or_error.t
