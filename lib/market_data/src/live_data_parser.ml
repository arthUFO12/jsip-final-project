open! Core
open Types
open Yojson.Safe.Util

(** {1 Venue category mappings} *)

(* Kalshi reports a category string on each event (e.g. "World",
   "Economics"). Map it onto our {!Category.t}; anything we don't track lands
   in [Miscellaneous]. *)
let kalshi_category_of_string (category : string) : Category.t =
  match String.lowercase category with
  | "climate" | "commodities" | "crypto" | "elections" | "mentions"
  | "politics" | "sports" ->
    Category.of_string category
  | "climate and weather" -> Category.Climate
  | "companies" -> Category.Finance
  | "economics" -> Category.Economy
  | "entertainment" -> Category.Culture
  | "financials" -> Category.Finance
  | "science and technology" -> Category.Tech
  | "social" -> Category.Culture
  | _ -> Category.Miscellaneous
;;

(* Polymarket does not expose a category field on gamma /markets; the [tags]
   array is the closest signal. Explicit tag_id -> category table — ids are
   stable, slugs are not. *)
let polymarket_category_of_tag_id : string -> Category.t = function
  | "2" -> Politics (* politics *)
  | "21" -> Crypto (* crypto *)
  | "120" -> Finance
  | "596" -> Culture (* pop-culture *)
  | "1401" -> Tech
  | "100265" -> Politics (* geopolitics: closest variant we track *)
  | "100639" -> Sports
  | _ -> Miscellaneous
;;

let polymarket_category (json : Yojson.Safe.t) : Category.t =
  let first_mapped_tag =
    match member "tags" json with
    | `List tags ->
      (* An unmapped tag must not stop the search — markets commonly lead
         with a niche tag ("bitcoin") before a mapped one ("crypto"). *)
      List.find_map tags ~f:(fun tag ->
        member "id" tag
        |> to_string_option
        |> Option.bind ~f:(fun id ->
          match polymarket_category_of_tag_id id with
          | Miscellaneous -> None
          | category -> Some category))
    | _ -> None
  in
  Option.value first_mapped_tag ~default:Category.Miscellaneous
;;

(** {1 Field helpers} *)

let json_price_opt_kalshi json key =
  json
  |> member key
  |> to_string_option
  |> Option.bind ~f:Price.of_dollars_string
;;

(* Gamma serializes whole numbers as JSON ints ("bestAsk": 1), which
   [to_float_option] rejects; accept both. *)
let to_number_opt json =
  match json with
  | `Int i -> Some (Float.of_int i)
  | `Float f -> Some f
  | `Null | `Bool _ | `String _ | `Intlit _ | `List _ | `Assoc _ | `Tuple _
  | `Variant _ ->
    None
;;

let json_price_opt_polymarket json key =
  json |> member key |> to_number_opt |> Option.map ~f:Price.of_float_dollars
;;

(* Kalshi's *_fp volume fields are decimal strings of contract counts, e.g.
   "114509.59"; round to a whole [Size.t]. *)
let json_volume_opt_kalshi json key =
  json
  |> member key
  |> to_string_option
  |> Option.bind ~f:(fun s ->
    Option.try_with (fun () ->
      Volume.Contracts
        (Size.of_int (Float.iround_nearest_exn (Float.of_string s)))))
;;

(* Polymarket's numeric volume fields (volumeNum, volume24hr) are USD
   notional floats. *)
let json_volume_opt_polymarket json key =
  json
  |> member key
  |> to_number_opt
  |> Option.map ~f:(fun v -> Volume.Notional (Price.of_float_dollars v))
;;

let time_of_json_opt json key =
  json
  |> member key
  |> to_string_option
  |> Option.bind ~f:(fun s ->
    Option.try_with (fun () -> Time_ns.of_string s))
;;

(** {1 Per-market parsers} *)

let parse_kalshi_market
  ~(category : string)
  ~(series_ticker : Slug.t option)
  (json : Yojson.Safe.t)
  : L1_market_metadata.t option
  =
  try
    Some
      { L1_market_metadata.venue = Venue.Kalshi
      ; market_id =
          json |> member "ticker" |> to_string |> Market_id.of_string
      ; title = json |> member "title" |> to_string
      ; slug = json |> member "ticker" |> to_string |> Slug.of_string
      ; event_slug =
          json
          |> member "event_ticker"
          |> to_string_option
          |> Option.map ~f:Slug.of_string
      ; series_ticker
      ; clob_token_id = None
      ; category = kalshi_category_of_string category
      ; yes_bid = json_price_opt_kalshi json "yes_bid_dollars"
      ; yes_ask = json_price_opt_kalshi json "yes_ask_dollars"
      ; last_price = json_price_opt_kalshi json "last_price_dollars"
      ; volume = json_volume_opt_kalshi json "volume_fp"
      ; volume_24h = json_volume_opt_kalshi json "volume_24h_fp"
      ; active = String.equal (json |> member "status" |> to_string) "active"
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
      ; market_id =
          json |> member "conditionId" |> to_string |> Market_id.of_string
      ; title = json |> member "question" |> to_string
      ; slug = json |> member "slug" |> to_string |> Slug.of_string
      ; event_slug =
          Option.try_with (fun () ->
            json
            |> member "events"
            |> index 0
            |> member "slug"
            |> to_string
            |> Slug.of_string)
      ; series_ticker = None
      ; clob_token_id =
          (* clobTokenIds is a doubly encoded JSON string:
             "[\"98022...\", \"53831...\"]"; index 0 = the Yes token. *)
          Option.try_with (fun () ->
            json
            |> member "clobTokenIds"
            |> to_string
            |> Yojson.Safe.from_string
            |> index 0
            |> to_string)
      ; category = polymarket_category json
      ; yes_bid = json_price_opt_polymarket json "bestBid"
      ; yes_ask = json_price_opt_polymarket json "bestAsk"
      ; last_price = json_price_opt_polymarket json "lastTradePrice"
      ; volume = json_volume_opt_polymarket json "volumeNum"
      ; volume_24h = json_volume_opt_polymarket json "volume24hr"
      ; active =
          json |> member "active" |> to_bool
          && not (json |> member "closed" |> to_bool)
      ; close_time = time_of_json_opt json "endDate"
      }
  with
  | Type_error (_, _) -> None
;;

(** {1 Top-level structure} *)

(* Kalshi wraps markets two deep: {"events": [{"category": ..,
   "series_ticker": .., "markets": [..]}, ..]}. Category and series ticker
   live on the event, so pair them with each nested market on the way
   out. *)
let kalshi_event_context_and_markets (fields : (string * Yojson.Safe.t) list)
  : (string * Slug.t option * Yojson.Safe.t) list
  =
  let events_list =
    List.Assoc.find_exn fields "events" ~equal:String.equal |> to_list
  in
  List.concat_map events_list ~f:(fun event ->
    (* Deep listing pages carry events with a null category; an unlabeled
       event must not kill the page. *)
    let category =
      member "category" event |> to_string_option |> Option.value ~default:""
    in
    let series_ticker =
      member "series_ticker" event
      |> to_string_option
      |> Option.map ~f:Slug.of_string
    in
    member "markets" event
    |> to_list
    |> List.map ~f:(fun market -> category, series_ticker, market))
;;

(* Report skipped entries, drop them, apply the limit. *)
let finalize ?limit parsed =
  let skipped = List.count parsed ~f:Option.is_none in
  if skipped > 0
  then
    eprintf
      "parse_data: skipped %d/%d entries\n"
      skipped
      (List.length parsed);
  let markets = List.filter_map parsed ~f:Fn.id in
  match limit with None -> markets | Some limit -> List.take markets limit
;;

let parse_data ?limit ~(venue : Venue.t) body =
  let open Or_error.Let_syntax in
  let%map parsed =
    match Yojson.Safe.from_string body with
    | exception _ -> Or_error.error_string "body is not valid JSON"
    | json ->
      (match venue, json with
       (* Kalshi wraps the data: {"events": [...], "cursor": ...} *)
       | Kalshi, `Assoc fields ->
         Or_error.try_with (fun () ->
           kalshi_event_context_and_markets fields
           |> List.map ~f:(fun (category, series_ticker, market) ->
             parse_kalshi_market ~category ~series_ticker market))
       (* Polymarket returns a bare top-level array *)
       | Polymarket, `List markets ->
         Ok (List.map markets ~f:parse_polymarket_market)
       | (Kalshi | Polymarket), _ ->
         Or_error.error_s
           [%message "unexpected top-level structure" (venue : Venue.t)])
  in
  finalize ?limit parsed
;;

(* public-search wraps markets one level down — {"events": [{"tags": ..,
   "markets": [..]}, ..]} — and the tags that drive the category live on
   the event, so graft them onto each market before the per-market parser
   runs. *)
let parse_polymarket_search ?limit body =
  let open Or_error.Let_syntax in
  let%map parsed =
    match Yojson.Safe.from_string body with
    | exception _ -> Or_error.error_string "body is not valid JSON"
    | json ->
      Or_error.try_with (fun () ->
        json
        |> member "events"
        |> to_list
        |> List.concat_map ~f:(fun event ->
          let tags = member "tags" event in
          member "markets" event
          |> to_list
          |> List.map ~f:(fun market ->
            match market with
            | `Assoc fields -> `Assoc (("tags", tags) :: fields)
            | market -> market))
        |> List.map ~f:parse_polymarket_market)
  in
  finalize ?limit parsed
;;

(* An exhausted listing answers with a missing or empty cursor. *)
let parse_kalshi_cursor body =
  match Yojson.Safe.from_string body with
  | exception _ -> None
  | json ->
    (match member "cursor" json |> to_string_option with
     | None | Some "" -> None
     | Some cursor -> Some cursor)
;;
