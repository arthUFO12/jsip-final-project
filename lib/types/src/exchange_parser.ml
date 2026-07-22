open! Core

let parse_kalshi_market (json : Yojson.Safe.t) : L1_market_metadata.t option =
  let open Yojson.Safe.Util in
  let price_opt key =
    json |> member key |> to_int_option |> Option.map ~f:Price.of_int_cents
  in
  try
    Some
      { L1_market_metadata.venue = Venue.Kalshi
      ; market_id = json |> member "ticker" |> to_string
      ; title = json |> member "title" |> to_string
      ; slug = json |> member "ticker" |> to_string
        (* Kalshi has no slug; reuse ticker. Swap for "event_ticker" if you
           want event-level grouping instead. *)
      ; yes_bid = price_opt "yes_bid"
      ; yes_ask = price_opt "yes_ask"
      ; last_price = price_opt "last_price"
      ; active =
          String.equal (json |> member "status" |> to_string) "active"
      ; close_time =
          json
          |> member "close_time"
          |> to_string_option
          |> Option.bind ~f:(fun s ->
            Option.try_with (fun () -> Time_ns.of_string s))
      }
  with
  | Type_error (_, _) -> None
;;

let parse_polymarket_market json : L1_market_metadata.t option =
  (* TODO: [outcomePrices] is a JSON array *encoded as a string*, e.g.
     "\"[\\\"0.97\\\", \\\"0.03\\\"]\"" — parse the string field, then
     parse *that* as JSON again. Index 0 = Yes, index 1 = No. TODO: convert
     decimal-dollar string -> Price.t (microcent field). *)
  ignore json;
  None
;;

(* converts the body from string to json for use in parse functions *)
let markets_of_body ~(body : string) ~(venue : Venue.t) : Yojson.Safe.t list Or_error.t =
  match Yojson.Safe.from_string body with
  | exception _ -> Or_error.error_string "body is not valid JSON"
  | json ->
    (match venue, json with
     (* Kalshi wraps the array: {"markets": [...], "cursor": ...} *)
     | Kalshi, `Assoc fields ->
       (match List.Assoc.find fields "markets" ~equal:String.equal with
        | Some (`List ms) -> Ok ms
        | _ -> Or_error.error_string "kalshi: missing \"markets\" array")
     (* Polymarket returns a bare top-level array *)
     | Polymarket, `List ms -> Ok ms
     | _, _ ->
       Or_error.error_s
         [%message "unexpected top-level structure" (venue : Venue.t)])
;;

let parse_data ~body ~(venue : Venue.t) =
  let open Or_error.Let_syntax in
  let%map markets = markets_of_body ~body ~venue in
  let parse_one =
    match venue with
    | Kalshi -> parse_kalshi_market
    | Polymarket -> parse_polymarket_market
  in
  (* Per-entry failures are skipped, not fatal. *)
  List.filter_map markets ~f:(fun m ->
    (* TODO (Kalshi): drop multi-leg/MVE entries here if any slip past
       [mve_filter=exclude] — check the relevant field before parsing. *)
    (* TODO: count/log skipped entries so schema drift is visible. *)
    parse_one m)
;;
