open! Core
open Types

(* Decimal share counts arrive as strings; only whole contracts are tradable
   on both legs at once, so floor. *)
let size_of_decimal_string text =
  Size.of_int (Int.of_float (Float.round_down (Float.of_string text)))
;;

let price_of_dollars_string_exn text =
  match Price.of_dollars_string text with
  | Some price -> price
  | None -> raise_s [%message "unparseable price" (text : string)]
;;

(* Kalshi rests every order as a bid on YES or NO, so the ask on one outcome
   is $1 - the best bid on the other, at that bid's count. *)
let kalshi_of_best_bids ~yes_bid ~no_bid =
  let (yes_bid : Price.t), yes_bid_size = yes_bid in
  let (no_bid : Price.t), no_bid_size = no_bid in
  { Binary_book.yes_ask = Price.( - ) Price.one no_bid
  ; yes_ask_size = no_bid_size
  ; no_ask = Price.( - ) Price.one yes_bid
  ; no_ask_size = yes_bid_size
  }
;;

let best_bid levels ~side =
  match
    List.max_elt levels ~compare:(Comparable.lift Price.compare ~f:fst)
  with
  | Some bid -> bid
  | None -> raise_s [%message "no resting bids" (side : string)]
;;

(* [orderbook_fp] levels: [["0.9300", "73.84"], ..] — dollars and shares as
   decimal strings. *)
let kalshi_fp_levels orderbook ~side =
  let open Yojson.Safe.Util in
  match member side orderbook with
  | `Null -> []
  | levels ->
    to_list levels
    |> List.map ~f:(fun level ->
      match to_list level with
      | [ price; size ] ->
        ( price_of_dollars_string_exn (to_string price)
        , size_of_decimal_string (to_string size) )
      | ([] | [ _ ] | _ :: _ :: _) as level ->
        raise_s
          [%message
            "expected [price, size] level"
              ~level:(Yojson.Safe.to_string (`List level) : string)])
;;

(* Legacy [orderbook] levels: [[51, 60], ..] — whole cents and contract
   counts as ints. *)
let kalshi_legacy_levels orderbook ~side =
  let open Yojson.Safe.Util in
  match member side orderbook with
  | `Null -> []
  | levels ->
    to_list levels
    |> List.map ~f:(fun level ->
      match to_list level with
      | [ price; count ] ->
        Price.of_int_cents (to_int price), Size.of_int (to_int count)
      | ([] | [ _ ] | _ :: _ :: _) as level ->
        raise_s
          [%message
            "expected [price, count] level"
              ~level:(Yojson.Safe.to_string (`List level) : string)])
;;

let parse_kalshi_book body =
  Or_error.tag
    ~tag:"could not parse kalshi orderbook"
    (Or_error.try_with (fun () ->
       let open Yojson.Safe.Util in
       let json = Yojson.Safe.from_string body in
       let bids ~side =
         match member "orderbook_fp" json with
         | `Null ->
           (match member "orderbook" json with
            | `Null ->
              (* A market with no resting orders at all serves a null book. *)
              raise_s [%message "orderbook is empty (null)"]
            | orderbook ->
              kalshi_legacy_levels orderbook ~side:[%string "%{side}"])
         | orderbook_fp ->
           kalshi_fp_levels orderbook_fp ~side:[%string "%{side}_dollars"]
       in
       kalshi_of_best_bids
         ~yes_bid:(best_bid (bids ~side:"yes") ~side:"yes")
         ~no_bid:(best_bid (bids ~side:"no") ~side:"no")))
;;

let parse_polymarket_book body =
  Or_error.tag
    ~tag:"could not parse polymarket book"
    (Or_error.try_with (fun () ->
       let open Yojson.Safe.Util in
       let book = Yojson.Safe.from_string body in
       let levels side =
         member side book
         |> to_list
         |> List.map ~f:(fun level ->
           ( price_of_dollars_string_exn (member "price" level |> to_string)
           , size_of_decimal_string (member "size" level |> to_string) ))
       in
       let best side ~compare =
         match List.max_elt (levels side) ~compare with
         | Some level -> level
         | None -> raise_s [%message "no resting orders" (side : string)]
       in
       let by_price = Comparable.lift Price.compare ~f:fst in
       (* Lowest ask = best ask; highest bid = best bid. *)
       let yes_ask, yes_ask_size =
         best "asks" ~compare:(Comparable.reverse by_price)
       in
       let yes_bid, yes_bid_size = best "bids" ~compare:by_price in
       { Binary_book.yes_ask
       ; yes_ask_size
       ; no_ask = Price.( - ) Price.one yes_bid
       ; no_ask_size = yes_bid_size
       }))
;;
