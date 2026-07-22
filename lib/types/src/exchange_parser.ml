open! Core
open Yojson.Safe.Util

let json_price_opt_kalshi json key =
  json |> member key |> to_int_option |> Option.map ~f:Price.of_int_cents
;;

let json_price_opt_polymarket json key =
  json
  |> member key
  |> to_float_option
  |> Option.map ~f:Price.of_float_dollars
;;

let time_of_json_opt json key =
  json
  |> member key
  |> to_string_option
  |> Option.bind ~f:(fun s ->
    Option.try_with (fun () -> Time_ns.of_string s))
;;

let parse_kalshi_market (json : Yojson.Safe.t) : L1_market_metadata.t option =
  try
    Some
      { L1_market_metadata.venue = Venue.Kalshi
      ; market_id = json |> member "ticker" |> to_string
      ; title = json |> member "title" |> to_string
      ; slug = json |> member "ticker" |> to_string
      ; event_slug = json |> member "event_ticker" |> to_string_option
      ; category = json |> member "category" |> to_string_option
      ; yes_bid = json_price_opt_kalshi json "yes_bid"
      ; yes_ask = json_price_opt_kalshi json "yes_ask"
      ; last_price = json_price_opt_kalshi json "last_price"
      ; active =
          String.equal (json |> member "status" |> to_string) "active"
          (* kalshi must derive boolean from string not being labeled active *)
      ; close_time = time_of_json_opt json "close_time"
      }
  with
  | Type_error (_, _) -> None
;;

let parse_polymarket_market (json : Yojson.Safe.t)
  : L1_market_metadata.t option
  =
  try
    Some
      { L1_market_metadata.venue = Venue.Polymarket
      ; market_id = json |> member "conditionId" |> to_string
      ; title = json |> member "question" |> to_string
      ; slug = json |> member "slug" |> to_string
      ; event_slug =
          Option.try_with (fun () ->
            json |> member "events" |> index 0 |> member "slug" |> to_string)
      ; category = json |> member "category" |> to_string_option
      ; yes_bid = json_price_opt_polymarket json "bestBid"
      ; yes_ask = json_price_opt_polymarket json "bestAsk"
      ; last_price = json_price_opt_polymarket json "lastTradePrice"
      ; active =
          json |> member "active" |> to_bool
          && not (json |> member "closed" |> to_bool)
      ; close_time = time_of_json_opt json "endDate"
      }
  with
  | Type_error (_, _) -> None
;;

(* converts the body from string to json for use in parse functions *)
let markets_of_body ~(body : string) ~(venue : Venue.t)
  : Yojson.Safe.t list Or_error.t
  =
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
  let parsed = List.map markets ~f:parse_one in
  let skipped = List.count parsed ~f:Option.is_none in
  if skipped > 0
  then
    eprintf
      "parse_data: skipped %d/%d entries\n"
      skipped
      (List.length markets);
  List.filter_map parsed ~f:Fn.id
;;
