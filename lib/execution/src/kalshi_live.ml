open! Core
open! Async
open Types

let live_host = "api.elections.kalshi.com"
let demo_host = "demo-api.kalshi.co"

(* The V2 order surface (the venue 410s the old /portfolio/orders create with
   "deprecated_v1_order_endpoint"): single-book orders — [bid]/[ask] on YES,
   fixed-point dollar strings — at /portfolio/events/orders. *)
let create_order_path = "/trade-api/v2/portfolio/events/orders"
let order_by_id_path order_id = [%string "%{create_order_path}/%{order_id}"]

(* Reads were not migrated with the create/cancel endpoints: GET-by-id only
   exists at the legacy path (the V2 one 404s), verified against the demo
   environment 2026-08-04. *)
let legacy_order_path order_id =
  [%string "/trade-api/v2/portfolio/orders/%{order_id}"]
;;

module Pss_sha256 = Mirage_crypto_pk.Rsa.PSS (Mirage_crypto.Hash.SHA256)

module Credentials = struct
  type t =
    { key_id : string
    ; private_key : Mirage_crypto_pk.Rsa.priv
    ; host : string
    }

  let create ?(host = live_host) ~key_id ~private_key_pem () =
    match
      X509.Private_key.decode_pem (Cstruct.of_string private_key_pem)
    with
    | Ok (`RSA private_key) -> Ok { key_id; private_key; host }
    | Ok (`ED25519 _ | `P224 _ | `P256 _ | `P384 _ | `P521 _) ->
      Or_error.error_string
        "kalshi private key must be an RSA key; got another key type"
    | Error (`Msg message) ->
      Or_error.error_s
        [%message
          "could not decode kalshi private key PEM" (message : string)]
  ;;

  let host t = t.host
  let key_id_env = "KALSHI_API_KEY_ID"
  let private_key_file_env = "KALSHI_PRIVATE_KEY_FILE"

  let missing_env vars =
    Deferred.Or_error.error_s
      [%message
        "missing environment variables for live kalshi trading"
          (vars : string list)]
  ;;

  (* The ssh posture: a venue key readable by other users is refused, not
     warned about. 0o077 masks the group/other bits. *)
  let check_key_file_permissions private_key_file =
    let open Deferred.Or_error.Let_syntax in
    let%bind stat =
      Monitor.try_with_or_error
        ~name:"stat kalshi private key file"
        (fun () -> Unix.stat private_key_file)
    in
    let loose_bits = stat.perm land 0o077 in
    match loose_bits = 0 with
    | true -> return ()
    | false ->
      Deferred.Or_error.error_s
        [%message
          "refusing group/world-readable kalshi private key; run: chmod 600 \
           <file>"
            (private_key_file : string)
            ~permissions:(sprintf "0o%o" stat.perm : string)]
  ;;

  let load_from_env ?host () =
    match
      Core.Sys.getenv key_id_env, Core.Sys.getenv private_key_file_env
    with
    | None, None -> missing_env [ key_id_env; private_key_file_env ]
    | None, Some (_ : string) -> missing_env [ key_id_env ]
    | Some (_ : string), None -> missing_env [ private_key_file_env ]
    | Some key_id, Some private_key_file ->
      let open Deferred.Or_error.Let_syntax in
      let%bind () = check_key_file_permissions private_key_file in
      let%bind private_key_pem =
        Monitor.try_with_or_error
          ~name:"read kalshi private key file"
          (fun () -> Reader.file_contents private_key_file)
      in
      Deferred.return (create ?host ~key_id ~private_key_pem ())
  ;;
end

let signing_input ~timestamp_ms ~verb ~path =
  [%string "%{timestamp_ms#Int}%{verb}%{path}"]
;;

let sign ~(credentials : Credentials.t) ~timestamp_ms ~verb ~path =
  Entropy.ensure ();
  Pss_sha256.sign
    ~key:credentials.private_key
    (`Message (Cstruct.of_string (signing_input ~timestamp_ms ~verb ~path)))
  |> Cstruct.to_string
  |> Base64.encode_string
;;

let verify
  ~(credentials : Credentials.t)
  ~signature
  ~timestamp_ms
  ~verb
  ~path
  =
  match Base64.decode signature with
  | Error (`Msg (_ : string)) -> false
  | Ok raw ->
    Pss_sha256.verify
      ~key:(Mirage_crypto_pk.Rsa.pub_of_priv credentials.private_key)
      ~signature:(Cstruct.of_string raw)
      (`Message
        (Cstruct.of_string (signing_input ~timestamp_ms ~verb ~path)))
;;

let order_body (order : Order.t) ~client_order_id =
  let open Or_error.Let_syntax in
  let%bind price_cents =
    match
      Or_error.try_with (fun () -> Price.to_int_cents_exn order.limit_price)
    with
    | Ok cents -> Ok cents
    | Error (_ : Error.t) ->
      Or_error.error_s
        [%message "kalshi prices are whole cents" (order : Order.t)]
  in
  let%bind () =
    if price_cents >= 1 && price_cents <= 99
    then Ok ()
    else
      Or_error.error_s
        [%message
          "kalshi limit must be between 1c and 99c"
            (price_cents : int)
            (order : Order.t)]
  in
  let%bind () =
    if Size.( > ) order.size Size.zero
    then Ok ()
    else
      Or_error.error_s
        [%message "order size must be positive" (order : Order.t)]
  in
  (* The V2 book is single-sided: [bid] buys YES, [ask] sells YES. A NO order
     maps through the binary identity — buying NO at [q] is selling YES at
     [$1 - q], and vice versa. *)
  let side, yes_price_cents =
    match order.contract, order.side with
    | Yes, Buy -> "bid", price_cents
    | Yes, Sell -> "ask", price_cents
    | No, Buy -> "ask", 100 - price_cents
    | No, Sell -> "bid", 100 - price_cents
  in
  (* Fixed-point strings, per the V2 schema: counts with 2 decimals, prices
     as dollars with 4. *)
  let count = sprintf "%d.00" (Size.to_int order.size) in
  let price = sprintf "0.%02d00" yes_price_cents in
  return
    (Yojson.Safe.to_string
       (`Assoc
         [ "ticker", `String (Market_id.to_string order.market.market_id)
         ; "client_order_id", `String client_order_id
         ; "side", `String side
         ; "count", `String count
         ; "price", `String price
         ; "time_in_force", `String "good_till_canceled"
         ; "self_trade_prevention_type", `String "taker_at_cross"
         ]))
;;

(* A V2 fixed-point count/dollar field ("10.00", "0.5600") as a float; fields
   the venue omits (fill-dependent ones) read as [None]. *)
let fixed_point json ~field =
  match Yojson.Safe.Util.member field json with
  | `String text -> Some (Float.of_string text)
  | `Int n -> Some (Float.of_int n)
  | `Float f -> Some f
  | _ -> None
;;

(* The synchronous half of the venue's answer (V2 shape, top-level):
   [order_id] always, [fill_count] contracts traded immediately, and — only
   when something filled — the average fill price and fee actually charged. A
   resting remainder shows up as [fill_count] short of the order's size;
   reconciling it later is future work. When the venue doesn't report a fee,
   it is our own computation of the published formula at the limit. *)
let parse_order_response (order : Order.t) ~body =
  Or_error.tag
    (Or_error.try_with (fun () ->
       let json = Yojson.Safe.from_string body in
       let venue_order_id =
         Yojson.Safe.Util.to_string (Yojson.Safe.Util.member "order_id" json)
       in
       let filled_size =
         Size.of_int
           (Int.of_float
              (Option.value
                 (fixed_point json ~field:"fill_count")
                 ~default:0.))
       in
       let price =
         match fixed_point json ~field:"average_fill_price" with
         | Some dollars -> Price.of_float_dollars dollars
         | None -> order.limit_price
       in
       let fee =
         match fixed_point json ~field:"average_fee_paid" with
         | Some dollars ->
           Size.multiply_by_price
             filled_size
             (Price.of_float_dollars dollars)
         | None ->
           Size.multiply_by_price
             filled_size
             (Fees.taker_fee Kalshi order.limit_price)
       in
       { Fill.order
       ; filled_size
       ; price
       ; fee
       ; venue_order_id = Some venue_order_id
       }))
    ~tag:"could not parse kalshi create-order response"
;;

let now_ms () = Time_ns.to_int_ns_since_epoch (Time_ns.now ()) / 1_000_000

(* Kalshi wants client order ids unique per order; timestamp plus a process
   counter survives two legs placed in the same millisecond. *)
let order_counter = ref 0

let next_client_order_id ~timestamp_ms =
  incr order_counter;
  [%string "arbiter-%{timestamp_ms#Int}-%{!order_counter#Int}"]
;;

(* One signed request to the venue: sign [timestamp ^ verb ^ path], send,
   return status and body. Every endpoint goes through here so the signing
   scheme lives in exactly one place. *)
let signed_request (credentials : Credentials.t) ~verb ~path ?body ~name () =
  let open Deferred.Or_error.Let_syntax in
  let timestamp_ms = now_ms () in
  let signature = sign ~credentials ~timestamp_ms ~verb ~path in
  let headers =
    Cohttp.Header.of_list
      [ "KALSHI-ACCESS-KEY", credentials.key_id
      ; "KALSHI-ACCESS-SIGNATURE", signature
      ; "KALSHI-ACCESS-TIMESTAMP", Int.to_string timestamp_ms
      ; "Content-Type", "application/json"
      ]
  in
  let uri = Uri.make ~scheme:"https" ~host:credentials.host ~path () in
  let%bind response, response_body =
    Monitor.try_with_or_error ~name (fun () ->
      match verb with
      | "POST" ->
        Cohttp_async.Client.post
          ~headers
          ~body:(Cohttp_async.Body.of_string (Option.value body ~default:""))
          uri
      | "GET" -> Cohttp_async.Client.get ~headers uri
      | "DELETE" -> Cohttp_async.Client.delete ~headers uri
      | verb -> raise_s [%message "unsupported http verb" (verb : string)])
  in
  let%bind response_body =
    Deferred.ok (Cohttp_async.Body.to_string response_body)
  in
  return (Cohttp.Response.status response, response_body)
;;

let place_order
  ?client_order_id
  (credentials : Credentials.t)
  (order : Order.t)
  =
  match Order.venue order with
  | Polymarket ->
    Deferred.Or_error.error_s
      [%message
        "kalshi_live cannot route a non-kalshi order" (order : Order.t)]
  | Kalshi ->
    let open Deferred.Or_error.Let_syntax in
    let client_order_id =
      match client_order_id with
      | Some id -> id
      | None -> next_client_order_id ~timestamp_ms:(now_ms ())
    in
    let%bind body = Deferred.return (order_body order ~client_order_id) in
    let%bind status, response_body =
      signed_request
        credentials
        ~verb:"POST"
        ~path:create_order_path
        ~body
        ~name:"kalshi create order"
        ()
    in
    (match status with
     | #Cohttp.Code.success_status ->
       Deferred.return (parse_order_response order ~body:response_body)
     | status ->
       Deferred.Or_error.error_s
         [%message
           "kalshi rejected the order"
             (Cohttp.Code.string_of_status status : string)
             (response_body : string)
             (order : Order.t)])
;;

let order_status (credentials : Credentials.t) ~order_id =
  let open Deferred.Or_error.Let_syntax in
  let path = legacy_order_path order_id in
  let%bind status, response_body =
    signed_request credentials ~verb:"GET" ~path ~name:"kalshi get order" ()
  in
  match status with
  | #Cohttp.Code.success_status ->
    Deferred.return
      (Or_error.tag
         (Or_error.try_with (fun () ->
            let json = Yojson.Safe.from_string response_body in
            (* The V2 get-order shape is undocumented; accept the status at
               top level or nested under "order". *)
            match Yojson.Safe.Util.member "status" json with
            | `String status -> status
            | _ ->
              Yojson.Safe.Util.member "order" json
              |> Yojson.Safe.Util.member "status"
              |> Yojson.Safe.Util.to_string))
         ~tag:"could not parse kalshi get-order response")
  | status ->
    Deferred.Or_error.error_s
      [%message
        "kalshi get-order failed"
          (Cohttp.Code.string_of_status status : string)
          (response_body : string)
          (order_id : string)]
;;

(* Cancels the resting remainder; contracts already filled stay filled.
   Returns the venue's answer of how many contracts the cancel removed. *)
let cancel_order (credentials : Credentials.t) ~order_id =
  let open Deferred.Or_error.Let_syntax in
  let path = order_by_id_path order_id in
  let%bind status, response_body =
    signed_request
      credentials
      ~verb:"DELETE"
      ~path
      ~name:"kalshi cancel order"
      ()
  in
  match status with
  | #Cohttp.Code.success_status ->
    Deferred.return
      (Or_error.tag
         (Or_error.try_with (fun () ->
            let json = Yojson.Safe.from_string response_body in
            Int.of_float
              (Option.value
                 (fixed_point json ~field:"reduced_by")
                 ~default:0.)))
         ~tag:"could not parse kalshi cancel-order response")
  | status ->
    Deferred.Or_error.error_s
      [%message
        "kalshi cancel failed"
          (Cohttp.Code.string_of_status status : string)
          (response_body : string)
          (order_id : string)]
;;

module For_testing = struct
  let signing_input = signing_input
  let sign = sign
  let verify = verify
  let order_body = order_body
  let parse_order_response = parse_order_response
end
