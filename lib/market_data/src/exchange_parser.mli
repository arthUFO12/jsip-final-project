open! Core
open Types

(** Decodes a raw venue payload into normalized L1 records.

    [body] is the untouched HTTP/WS response body. Venue quirks are handled
    internally:
    - Kalshi: expects the /events?with_nested_markets=true shape
      ({[ {"events": [{"category": .., "markets": [..]}, ..]} ]}); the
      event-level category is attached to each nested market.
    - Polymarket: expects the bare top-level array from gamma /markets;
      the category is derived from a hardcoded tag-id table over the
      [tags] array (fetch with [include_tag=true]).

    [limit] caps the number of parsed records returned (useful for Kalshi,
    where the fetch-side limit counts events, not markets).

    Returns [Error _] if [body] is not valid JSON or lacks the expected
    top-level structure. Individual malformed market entries within an
    otherwise-valid payload are skipped, not fatal. *)
val parse_data
  :  ?limit:int
  -> venue:Venue.t
  -> string (* body *)
  -> L1_market_metadata.t list Or_error.t
