open! Core
open! Async
open! Types
open Execution

(* A throwaway RSA key generated for this test file — it has never touched a
   real account and never will. Kalshi issues keys in this same PKCS#8 PEM
   form. *)
let test_key_pem =
  {|-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQCrUVCisTSwLObe
X5tg3rmmukPgVUJm6X1eBdJSLFrZUqOGaxqP3yQn94Tl96rN3aYu9gi8ZZ8KCO8+
4QO1E4w+hTME5nw510qTpr+uxyoyD5TDilzaLHa7WAXXjFY8gbxexdxbohpYXmWu
ozJxkC/UUsJpXEVbl1Xd6OXmO/OpEJF2yPz60eAa8KmZle9vjurOansQf42oQh5J
SGNBM0NU04/v0579yTdLJR0izOAfKdS8NWiM+z6GZX/3ya2SwmGqdzUyIDhIJ4u9
6Gge+pjbWuWh+TQVGcWy4kI0eOfRWCBcIXSFNgjA0Ic635Q0asjVjGQORTINPPbK
ZrinQ2ENAgMBAAECggEAAP/qSnr7tvGfAwRWAN9nXEmP1zvQcv+5RkseheJZiM2M
bNmNgTnvSdD5rLtgilCguxTmpqtC+caOAtZMnaipPgUIRvYkn7WKZioL8yJmBg3r
Ll5xV1exW9UFUaBLCHL+ZNGpHGqSwfEhT6+B0qCATaruXFOMuwTUtdHDyk4o+O07
S8JZhBM4ErHjU8czA0ieYSM6XuDjpJR3AERMF2v7vcBHt5GmmYAXXBHsPMgWPk6/
6RSq16gjQ4pKYA9KpN2U2fkRliwpHXpTqij7aPupqh3I3LqsetefuuAMYT6WViIc
ohinSyFj41n2ej72SdLl3Yc4+sBySbVUJCBjaluALwKBgQDweCFeg3dLzZW32Fzc
U4rmIsIGpWGdJ8vrdFQjt0RgNJY/pKTVwuZhX9Kt0KRsqEuKt1rmZb/3tJ3aA5X/
yP48/lVOzAMJhKnG6iOFlgdinwoDIuPoso9DdkXrlxRiLy7xCq/mfy9ivQLa0qWe
OtxBgnKhcIVefqHxeZzkmOEWBwKBgQC2YdiHusEn7U5YLA7kz3CK7p82E91dvsQo
n0EZUMSeMVJ9GUCIiwkIZIoEwnGdjD2rZj4MmaKwOlV2IxH23TPYSCXIb06YHkAF
D2xhcLw1CABT3D4VOz5Vc/x/TpjX5D2VoILYmCvo0m7Zxk6fe1AowhJrD0rkY8qp
8VzfPrtrSwKBgEKxDpAn5e4gFmXmm9p/UQaCnU06kNgFMYkbujotmckUzJbaEt02
MK7Q5J1QQEniYxEVySAdGCCa9yx/0hoepGRGJWK1MiJxqKSCS1fBREsV0LEN/CeN
Xi5Xnwy32e9hJqOzUxWaMIox4ZjO0yAPMwb4JtKfYl3SwRc3o0vDGqoBAoGBAJZc
CHWt47yVCffrKsQ8uN3xaFTe/6tfsLyEvtiuG8sHgqgb/3YSmfnPWXIfFCf9DIKY
IiOlLdj33qkstIy/RRTUNkIOcc87cTA6zPFwTdmypQJ+GrjKRNOSceJK2/tw8cy/
rd//ZIPnqPflY8Bbm3Reu2yECQxNsTF2CUkLoNwNAoGBAKvIX9cBkyDbtM1Fx8qY
Su9gkMmOfJfE6SRL/vO9+Dfc8udiJv5YGKe71gQmqyAbBPntOgISJYpbliYNBVFH
wB5cci8UsGd0eiKZxVdnvYlY9GHEidPZTc8HRIez2lCl/d807+wWlQrbacNPSBmm
B0znxdAf68CPgUGIyUw2il9w
-----END PRIVATE KEY-----|}
;;

let credentials =
  Kalshi_live.Credentials.create
    ~key_id:"test-key-id"
    ~private_key_pem:test_key_pem
    ()
  |> Or_error.ok_exn
;;

let stub ~venue ~id =
  { Market_stub.venue
  ; market_id = Market_id.of_string id
  ; slug = Slug.of_string id
  ; series_ticker = None
  ; clob_token_id = None
  ; title = "test market"
  ; category = Crypto
  ; created_time = Time_ns.epoch
  ; close_time = Time_ns.of_string "2030-01-01 00:00:00Z"
  ; volume = None
  }
;;

let order ~limit_price ~size =
  { Order.market = stub ~venue:Kalshi ~id:"KXBTC-25DEC31-B100000"
  ; contract = Yes
  ; side = Buy
  ; limit_price
  ; size = Size.of_int size
  }
;;

let%expect_test "the signing input is exactly timestamp ^ verb ^ path" =
  print_endline
    (Kalshi_live.For_testing.signing_input
       ~timestamp_ms:1700000000000
       ~verb:"POST"
       ~path:"/trade-api/v2/portfolio/orders");
  [%expect {| 1700000000000POST/trade-api/v2/portfolio/orders |}];
  return ()
;;

(* PSS signatures are salted so their bytes are not stable; the durable
   property is that the public half of the key accepts them — which is what
   Kalshi's server does with the header. *)
let%test "a fresh signature verifies against the key's public half" =
  let timestamp_ms = 1700000000000 in
  let verb = "POST" in
  let path = "/trade-api/v2/portfolio/orders" in
  let signature =
    Kalshi_live.For_testing.sign ~credentials ~timestamp_ms ~verb ~path
  in
  Kalshi_live.For_testing.verify
    ~credentials
    ~signature
    ~timestamp_ms
    ~verb
    ~path
;;

let%test "a signature does not verify for a different timestamp" =
  let verb = "POST" in
  let path = "/trade-api/v2/portfolio/orders" in
  let signature =
    Kalshi_live.For_testing.sign
      ~credentials
      ~timestamp_ms:1700000000000
      ~verb
      ~path
  in
  not
    (Kalshi_live.For_testing.verify
       ~credentials
       ~signature
       ~timestamp_ms:1700000000001
       ~verb
       ~path)
;;

let%expect_test "a routable order becomes the venue's JSON shape" =
  print_s
    [%sexp
      (Kalshi_live.For_testing.order_body
         (order ~limit_price:(Price.of_int_cents 49) ~size:7)
         ~client_order_id:"arbiter-1700000000000-1"
       : string Or_error.t)];
  [%expect
    {|
    (Ok
     "{\"ticker\":\"KXBTC-25DEC31-B100000\",\"client_order_id\":\"arbiter-1700000000000-1\",\"side\":\"bid\",\"count\":\"7.00\",\"price\":\"0.4900\",\"time_in_force\":\"good_till_canceled\",\"self_trade_prevention_type\":\"taker_at_cross\"}")
    |}];
  return ()
;;

let%expect_test "orders kalshi cannot price are rejected before any network \
                 traffic: sub-cent limits, out-of-range limits, empty size"
  =
  let print_error ~limit_price ~size =
    print_s
      [%sexp
        (Or_error.ignore_m
           (Kalshi_live.For_testing.order_body
              (order ~limit_price ~size)
              ~client_order_id:"unused")
         : unit Or_error.t)]
  in
  print_error
    ~limit_price:(Price.of_microcents 45_500_000)
    ~size:7 (* 45.5c *);
  print_error ~limit_price:(Price.of_int_cents 100) ~size:7;
  print_error ~limit_price:(Price.of_int_cents 49) ~size:0;
  [%expect
    {|
    (Error
     ("kalshi prices are whole cents"
      (order
       ((market
         ((venue Kalshi) (market_id KXBTC-25DEC31-B100000)
          (slug KXBTC-25DEC31-B100000) (series_ticker ()) (clob_token_id ())
          (title "test market") (category Crypto)
          (created_time (1970-01-01 00:00:00.000000000Z))
          (close_time (2030-01-01 00:00:00.000000000Z)) (volume ())))
        (contract Yes) (side Buy) (limit_price 45500000) (size 7)))))
    (Error
     ("kalshi limit must be between 1c and 99c" (price_cents 100)
      (order
       ((market
         ((venue Kalshi) (market_id KXBTC-25DEC31-B100000)
          (slug KXBTC-25DEC31-B100000) (series_ticker ()) (clob_token_id ())
          (title "test market") (category Crypto)
          (created_time (1970-01-01 00:00:00.000000000Z))
          (close_time (2030-01-01 00:00:00.000000000Z)) (volume ())))
        (contract Yes) (side Buy) (limit_price 100000000) (size 7)))))
    (Error
     ("order size must be positive"
      (order
       ((market
         ((venue Kalshi) (market_id KXBTC-25DEC31-B100000)
          (slug KXBTC-25DEC31-B100000) (series_ticker ()) (clob_token_id ())
          (title "test market") (category Crypto)
          (created_time (1970-01-01 00:00:00.000000000Z))
          (close_time (2030-01-01 00:00:00.000000000Z)) (volume ())))
        (contract Yes) (side Buy) (limit_price 49000000) (size 0)))))
    |}];
  return ()
;;

let%expect_test "the venue's create-order response becomes a fill: order id \
                 kept, fixed-point fill count and venue-reported average \
                 price and fee trusted"
  =
  let order = order ~limit_price:(Price.of_int_cents 49) ~size:10 in
  let body =
    {|{"order_id":"abc-123","client_order_id":"arbiter-1","fill_count":"7.00","remaining_count":"3.00","average_fill_price":"0.4900","average_fee_paid":"0.0200","ts_ms":1700000000000}|}
  in
  print_s
    [%sexp
      (Kalshi_live.For_testing.parse_order_response order ~body
       : Fill.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((order
       ((market
         ((venue Kalshi) (market_id KXBTC-25DEC31-B100000)
          (slug KXBTC-25DEC31-B100000) (series_ticker ()) (clob_token_id ())
          (title "test market") (category Crypto)
          (created_time (1970-01-01 00:00:00.000000000Z))
          (close_time (2030-01-01 00:00:00.000000000Z)) (volume ())))
        (contract Yes) (side Buy) (limit_price 49000000) (size 10)))
      (filled_size 7) (price 49000000) (fee 14000000) (venue_order_id (abc-123))))
    |}];
  (* A resting order has no fills yet, and the venue omits the fill-dependent
     fields. *)
  let body =
    {|{"order_id":"abc-124","fill_count":"0.00","remaining_count":"10.00","ts_ms":1700000000001}|}
  in
  print_s
    [%sexp
      (Kalshi_live.For_testing.parse_order_response order ~body
       : Fill.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((order
       ((market
         ((venue Kalshi) (market_id KXBTC-25DEC31-B100000)
          (slug KXBTC-25DEC31-B100000) (series_ticker ()) (clob_token_id ())
          (title "test market") (category Crypto)
          (created_time (1970-01-01 00:00:00.000000000Z))
          (close_time (2030-01-01 00:00:00.000000000Z)) (volume ())))
        (contract Yes) (side Buy) (limit_price 49000000) (size 10)))
      (filled_size 0) (price 49000000) (fee 0) (venue_order_id (abc-124))))
    |}];
  print_s
    [%sexp
      (Or_error.ignore_m
         (Kalshi_live.For_testing.parse_order_response
            order
            ~body:"not json")
       : unit Or_error.t)];
  [%expect
    {|
    (Error
     ("could not parse kalshi create-order response"
      ("Yojson__Common.Json_error(\"Line 1, bytes 0-8:\\nInvalid token 'not json'\")")))
    |}];
  return ()
;;

let%expect_test "balance response parses from integer cents or fixed-point" =
  List.iter
    [ {|{"balance": 250075}|}; {|{"balance": "2500.75"}|}; {|{"nope": 1}|} ]
    ~f:(fun body ->
      print_s
        [%sexp
          (Kalshi_live.For_testing.parse_balance_response body
           : float Or_error.t)]);
  [%expect
    {|
    (Ok 2500.75)
    (Ok 2500.75)
    (Error
     ("could not parse kalshi balance response"
      ("no balance field in response" (body "{\"nope\": 1}"))))
    |}];
  return ()
;;

let%expect_test "positions response keeps open rows and drops flat ones" =
  let body =
    {|{"event_positions": [],
       "market_positions":
         [ {"ticker": "KXFED-A", "position": 12, "market_exposure": 480},
           {"ticker": "KXBTC-B", "position": 0, "market_exposure": 0},
           {"ticker": "KXCPI-C", "position": -3, "market_exposure": 150} ]}|}
  in
  print_s
    [%sexp
      (Kalshi_live.For_testing.parse_positions_response body
       : Kalshi_live.Position.t list Or_error.t)];
  [%expect
    {|
    (Ok
     (((ticker KXFED-A) (position 12) (exposure_dollars 4.8))
      ((ticker KXCPI-C) (position -3) (exposure_dollars 1.5))))
    |}];
  print_s
    [%sexp
      (Kalshi_live.For_testing.parse_positions_response {|{"other": 1}|}
       : Kalshi_live.Position.t list Or_error.t)];
  [%expect {| (Ok ()) |}];
  return ()
;;
