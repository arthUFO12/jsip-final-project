open! Core
open! Async
open Types

let host = "api.elections.kalshi.com"
let create_order_path = "/trade-api/v2/portfolio/orders"

(* PSS salts are random, and there is no mirage-crypto-rng-unix in the switch
   to plumb entropy automatically — so seed Fortuna from the kernel once, on
   the first signature. *)
let rng_seed_length = 32

let initialize_rng =
  lazy
    (let seed =
       In_channel.with_file "/dev/urandom" ~f:(fun channel ->
         let buf = Bytes.create rng_seed_length in
         match
           In_channel.really_input channel ~buf ~pos:0 ~len:rng_seed_length
         with
         | Some () -> Bytes.to_string buf
         | None -> raise_s [%message "unexpected end of /dev/urandom"])
     in
     Mirage_crypto_rng.set_default_generator
       (Mirage_crypto_rng.create
          ~seed:(Cstruct.of_string seed)
          (module Mirage_crypto_rng.Fortuna)))
;;

module Pss_sha256 = Mirage_crypto_pk.Rsa.PSS (Mirage_crypto.Hash.SHA256)

module Credentials = struct
  type t =
    { key_id : string
    ; private_key : Mirage_crypto_pk.Rsa.priv
    }

  let create ~key_id ~private_key_pem =
    match
      X509.Private_key.decode_pem (Cstruct.of_string private_key_pem)
    with
    | Ok (`RSA private_key) -> Ok { key_id; private_key }
    | Ok (`ED25519 _ | `P224 _ | `P256 _ | `P384 _ | `P521 _) ->
      Or_error.error_string
        "kalshi private key must be an RSA key; got another key type"
    | Error (`Msg message) ->
      Or_error.error_s
        [%message
          "could not decode kalshi private key PEM" (message : string)]
  ;;

  let key_id_env = "KALSHI_API_KEY_ID"
  let private_key_file_env = "KALSHI_PRIVATE_KEY_FILE"

  let missing_env vars =
    Deferred.Or_error.error_s
      [%message
        "missing environment variables for live kalshi trading"
          (vars : string list)]
  ;;

  let load_from_env () =
    match
      Core.Sys.getenv key_id_env, Core.Sys.getenv private_key_file_env
    with
    | None, None -> missing_env [ key_id_env; private_key_file_env ]
    | None, Some (_ : string) -> missing_env [ key_id_env ]
    | Some (_ : string), None -> missing_env [ private_key_file_env ]
    | Some key_id, Some private_key_file ->
      let%map pem =
        Monitor.try_with_or_error
          ~name:"read kalshi private key file"
          (fun () -> Reader.file_contents private_key_file)
      in
      Or_error.bind pem ~f:(fun private_key_pem ->
        create ~key_id ~private_key_pem)
  ;;
end

let signing_input ~timestamp_ms ~verb ~path =
  [%string "%{timestamp_ms#Int}%{verb}%{path}"]
;;

let sign ~(credentials : Credentials.t) ~timestamp_ms ~verb ~path =
  force initialize_rng;
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
  let action = match order.side with Buy -> "buy" | Sell -> "sell" in
  let side, price_field =
    match order.contract with
    | Yes -> "yes", "yes_price"
    | No -> "no", "no_price"
  in
  return
    (Yojson.Safe.to_string
       (`Assoc
         [ "ticker", `String (Market_id.to_string order.market.market_id)
         ; "client_order_id", `String client_order_id
         ; "action", `String action
         ; "side", `String side
         ; "count", `Int (Size.to_int order.size)
         ; "type", `String "limit"
         ; price_field, `Int price_cents
         ]))
;;

(* The synchronous half of the venue's answer: order id always, plus how many
   contracts traded immediately as taker. A resting remainder shows up as
   [filled_size] short of the order's size; reconciling it later is future
   work. The fee is our own computation of the venue's published formula at
   the limit price. *)
let parse_order_response (order : Order.t) ~body =
  Or_error.tag
    (Or_error.try_with (fun () ->
       let json = Yojson.Safe.from_string body in
       let order_json = Yojson.Safe.Util.member "order" json in
       let venue_order_id =
         Yojson.Safe.Util.to_string
           (Yojson.Safe.Util.member "order_id" order_json)
       in
       let taker_fill_count =
         Option.value
           ~default:0
           (Yojson.Safe.Util.to_int_option
              (Yojson.Safe.Util.member "taker_fill_count" order_json))
       in
       let filled_size = Size.of_int taker_fill_count in
       { Fill.order
       ; filled_size
       ; price = order.limit_price
       ; fee =
           Size.multiply_by_price
             filled_size
             (Fees.taker_fee Kalshi order.limit_price)
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

let place_order (credentials : Credentials.t) (order : Order.t) =
  match Order.venue order with
  | Polymarket ->
    Deferred.Or_error.error_s
      [%message
        "kalshi_live cannot route a non-kalshi order" (order : Order.t)]
  | Kalshi ->
    let open Deferred.Or_error.Let_syntax in
    let timestamp_ms = now_ms () in
    let%bind body =
      Deferred.return
        (order_body
           order
           ~client_order_id:(next_client_order_id ~timestamp_ms))
    in
    let signature =
      sign ~credentials ~timestamp_ms ~verb:"POST" ~path:create_order_path
    in
    let headers =
      Cohttp.Header.of_list
        [ "KALSHI-ACCESS-KEY", credentials.key_id
        ; "KALSHI-ACCESS-SIGNATURE", signature
        ; "KALSHI-ACCESS-TIMESTAMP", Int.to_string timestamp_ms
        ; "Content-Type", "application/json"
        ]
    in
    let%bind response, response_body =
      Monitor.try_with_or_error ~name:"kalshi create order" (fun () ->
        Cohttp_async.Client.post
          ~headers
          ~body:(Cohttp_async.Body.of_string body)
          (Uri.make ~scheme:"https" ~host ~path:create_order_path ()))
    in
    let%bind response_body =
      Deferred.ok (Cohttp_async.Body.to_string response_body)
    in
    (match Cohttp.Response.status response with
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

module For_testing = struct
  let signing_input = signing_input
  let sign = sign
  let verify = verify
  let order_body = order_body
  let parse_order_response = parse_order_response
end
