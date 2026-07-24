open! Core
open Types
open Yojson.Safe.Util



type market_metadata = 
{ category : string 
; kalshi_series_ticker : Slug.t option

}
let json_price_opt_kalshi json key =
  json
  |> member key
  |> to_string_option
  |> Option.bind ~f:Price.of_dollars_string
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

let parse_kalshi_market (metadata : market_metadata) (json : Yojson.Safe.t) : L1_market_metadata.t option =
  try
    Some
      { L1_market_metadata.venue = Venue.Kalshi
      ; market_id = json |> member "ticker" |> to_string |> Market_id.of_string
      ; title = json |> member "title" |> to_string
      ; slug = json |> member "ticker" |> to_string |> Slug.of_string
      ; event_slug = json |> member "event_ticker" |> to_string_option |> Option.map ~f:Slug.of_string
      ; category = Some (Category.of_string metadata.category)
      ; yes_bid = json_price_opt_kalshi json "yes_bid_dollars"
      ; yes_ask = json_price_opt_kalshi json "yes_ask_dollars"
      ; last_price = json_price_opt_kalshi json "last_price_dollars"
      ; active = String.equal (json |> member "status" |> to_string) "active"
      ; close_time = time_of_json_opt json "close_time"
      }
  with
  | Type_error (_, _) -> None
;;

let parse_polymarket_market (_category : string) (json : Yojson.Safe.t)
  : L1_market_metadata.t option
  =
  try
    Some
      { L1_market_metadata.venue = Venue.Polymarket
      ; market_id = json |> member "conditionId" |> to_string |> Market_id.of_string
      ; title = json |> member "question" |> to_string 
      ; slug = json |> member "slug" |> to_string |> Slug.of_string
      ; event_slug =
          Option.try_with (fun () ->
            json |> member "events" |> index 0 |> member "slug" |> to_string |> Slug.of_string)
      ; category = json |> member "category" |> to_string_option |> Category.of_string
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

let list_or_fail (json_obj : Yojson.Safe.t) : 'a list =
  match json_obj with
  | `List l -> l
  | _ -> failwith "Type error: Expected list type in json"
;;

let get_metadata_and_markets (fields : (string * Yojson.Safe.t) list)
  : (market_metadata * Yojson.Safe.t) list
  =
  let events_list =
    List.Assoc.find_exn fields "events" ~equal:String.equal |> list_or_fail
  in
  List.concat_map events_list ~f:(fun event ->
    let category = member "category" event |> to_string in
    let series_ticker = member "series_ticker" event |> to_string in
    member "markets" event
    |> list_or_fail
    |> List.map ~f:(fun market -> { category; kalshi_series_ticker = Some (Slug.of_string series_ticker) }, market))
;;

(* converts the body from string to json for use in parse functions *)
let markets_of_body ~(body : string) ~(venue : Venue.t)
  : (market_metadata * Yojson.Safe.t) list Or_error.t
  =
  match Yojson.Safe.from_string body with
  | exception _ -> Or_error.error_string "body is not valid JSON"
  | json ->
    (match venue, json with
     (* Kalshi wraps the array: {"markets": [...], "cursor": ...} *)
     | Kalshi, `Assoc fields ->
       Or_error.try_with (fun () -> get_metadata_and_markets fields)
     (* Polymarket returns a bare top-level array *)
     | _, _ ->
       Or_error.error_s
         [%message "unexpected top-level structure" (venue : Venue.t)])
;;

let parse_data ~body ~(venue : Venue.t) =
  let open Or_error.Let_syntax in
  let%map metadata_and_markets = markets_of_body ~body ~venue in
  let parse_one =
    match venue with
    | Kalshi -> parse_kalshi_market
    | Polymarket -> parse_polymarket_market
  in
  let parsed = List.map metadata_and_markets ~f:(fun (metadata, market) -> parse_one metadata market) in
  let skipped = List.count parsed ~f:Option.is_none in
  if skipped > 0
  then
    eprintf
      "parse_data: skipped %d/%d entries\n"
      skipped
      (List.length metadata_and_markets);
  List.filter_map parsed ~f:Fn.id
;;
