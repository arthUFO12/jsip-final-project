open! Core
open! Async
open! Types
open Execution

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
  }
;;

let order ~venue ~id =
  { Order.market = stub ~venue ~id
  ; contract = Yes
  ; side = Buy
  ; limit_price = Price.of_int_cents 45
  ; size = Size.of_int 10
  }
;;

let%expect_test "a paper executor is the simulator behind the shared \
                 interface — same call a live strategy makes, no venue \
                 order id in the fill"
  =
  let executor =
    Executor.paper
      (Simulator.create
         ~fill_model:At_limit
         ~starting_cash:(Price.of_int_cents 1000))
  in
  print_s [%sexp (Executor.is_live executor : bool)];
  let%bind fill =
    Executor.place_order executor (order ~venue:Kalshi ~id:"K1")
  in
  (match fill with
   | Error error -> print_s [%message "rejected" ~_:(error : Error.t)]
   | Ok { Fill.order = _; filled_size; price; fee; venue_order_id } ->
     print_s
       [%message
         (filled_size : Size.t)
           (price : Price.t)
           (fee : Price.t)
           (venue_order_id : string option)]);
  [%expect
    {|
    false
    ((filled_size 10) (price 45000000) (fee 0) (venue_order_id ()))
    |}];
  return ()
;;

let%expect_test "a live executor refuses polymarket orders outright — no \
                 order can leave for a venue we cannot sign for"
  =
  let executor = Executor.live Test_kalshi_live.credentials in
  print_s [%sexp (Executor.is_live executor : bool)];
  let%bind fill =
    Executor.place_order executor (order ~venue:Polymarket ~id:"P1")
  in
  print_s [%sexp (Or_error.ignore_m fill : unit Or_error.t)];
  [%expect
    {|
    true
    (Error
     ("polymarket live trading is not supported: order placement needs an EIP-712 wallet signature; run paper mode for polymarket legs"
      (order
       ((market
         ((venue Polymarket) (market_id P1) (slug P1) (series_ticker ())
          (clob_token_id ()) (title "test market") (category Crypto)
          (created_time (1970-01-01 00:00:00.000000000Z))
          (close_time (2030-01-01 00:00:00.000000000Z))))
        (contract Yes) (side Buy) (limit_price 45000000) (size 10)))))
    |}];
  return ()
;;
