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
  ; volume = None
  }
;;

let order ~venue ~id ~contract ~side ~limit_cents ~size =
  { Order.market = stub ~venue ~id
  ; contract
  ; side
  ; limit_price = Price.of_int_cents limit_cents
  ; size = Size.of_int size
  }
;;

(* The order echoes back in every fill; print only what the fill decided. *)
let print_fill = function
  | Error error -> print_s [%message "rejected" ~_:(error : Error.t)]
  | Ok { Fill.order = _; filled_size; price; fee; venue_order_id } ->
    print_s
      [%message
        (filled_size : Size.t)
          (price : Price.t)
          (fee : Price.t)
          (venue_order_id : string option)]
;;

let print_state simulator ~market_id =
  print_s
    [%message
      ""
        ~cash:(Simulator.cash simulator : Price.t)
        ~yes:(Simulator.position simulator ~market_id ~contract:Yes : Size.t)
        ~no:(Simulator.position simulator ~market_id ~contract:No : Size.t)]
;;

let%expect_test "At_limit fills the whole order at the limit — buys debit \
                 cash, sells credit it, and the position follows"
  =
  let simulator =
    Simulator.create
      ~fill_model:At_limit
      ~starting_cash:(Price.of_int_cents 1000)
  in
  let market_id = Market_id.of_string "K1" in
  let%bind fill =
    Simulator.place_order
      simulator
      (order
         ~venue:Kalshi
         ~id:"K1"
         ~contract:Yes
         ~side:Buy
         ~limit_cents:45
         ~size:10)
  in
  print_fill fill;
  print_state simulator ~market_id;
  [%expect
    {|
    ((filled_size 10) (price 45000000) (fee 0) (venue_order_id ()))
    ((cash 550000000) (yes 10) (no 0))
    |}];
  let%bind fill =
    Simulator.place_order
      simulator
      (order
         ~venue:Kalshi
         ~id:"K1"
         ~contract:Yes
         ~side:Sell
         ~limit_cents:45
         ~size:4)
  in
  print_fill fill;
  print_state simulator ~market_id;
  [%expect
    {|
    ((filled_size 4) (price 45000000) (fee 0) (venue_order_id ()))
    ((cash 730000000) (yes 6) (no 0))
    |}];
  return ()
;;

let%expect_test "the simulator does not short: selling more than held is an \
                 error and changes nothing"
  =
  let simulator =
    Simulator.create
      ~fill_model:At_limit
      ~starting_cash:(Price.of_int_cents 1000)
  in
  let%bind fill =
    Simulator.place_order
      simulator
      (order
         ~venue:Kalshi
         ~id:"K1"
         ~contract:No
         ~side:Sell
         ~limit_cents:30
         ~size:1)
  in
  print_fill fill;
  print_state simulator ~market_id:(Market_id.of_string "K1");
  [%expect
    {|
    (rejected
     ("cannot sell more than held; the simulator does not short" (held 0)
      (order
       ((market
         ((venue Kalshi) (market_id K1) (slug K1) (series_ticker ())
          (clob_token_id ()) (title "test market") (category Crypto)
          (created_time (1970-01-01 00:00:00.000000000Z))
          (close_time (2030-01-01 00:00:00.000000000Z)) (volume ())))
        (contract No) (side Sell) (limit_price 30000000) (size 1)))))
    ((cash 1000000000) (yes 0) (no 0))
    |}];
  return ()
;;

let%expect_test "running out of simulated cash is an error, not a fill" =
  let simulator =
    Simulator.create
      ~fill_model:At_limit
      ~starting_cash:(Price.of_int_cents 100)
  in
  let%bind fill =
    Simulator.place_order
      simulator
      (order
         ~venue:Kalshi
         ~id:"K1"
         ~contract:Yes
         ~side:Buy
         ~limit_cents:45
         ~size:10)
  in
  print_fill fill;
  [%expect
    {|
    (rejected
     ("insufficient simulated cash" (cost 450000000) (cash 100000000)
      (fill.order
       ((market
         ((venue Kalshi) (market_id K1) (slug K1) (series_ticker ())
          (clob_token_id ()) (title "test market") (category Crypto)
          (created_time (1970-01-01 00:00:00.000000000Z))
          (close_time (2030-01-01 00:00:00.000000000Z)) (volume ())))
        (contract Yes) (side Buy) (limit_price 45000000) (size 10)))))
    |}];
  return ()
;;

(* The book-aware model, driven through its pure core so no network is
   involved: 49c YES ask with 30 up, 60c NO ask with 50 up. *)
let book =
  { Binary_book.yes_ask = Price.of_int_cents 49
  ; yes_ask_size = Size.of_int 30
  ; no_ask = Price.of_int_cents 60
  ; no_ask_size = Size.of_int 50
  }
;;

let%expect_test "against the book: fills at the ask (not the limit), clamps \
                 to the depth there, and charges the venue's taker fee"
  =
  let simulator =
    Simulator.create
      ~fill_model:Against_live_book
      ~starting_cash:(Price.of_int_cents 2000)
  in
  (* Willing to pay 52c for 40; the book has 30 at 49c. Kalshi's fee at 49c
     is 2c per contract. *)
  let fill =
    Simulator.For_testing.fill_against_book
      simulator
      (order
         ~venue:Kalshi
         ~id:"K1"
         ~contract:Yes
         ~side:Buy
         ~limit_cents:52
         ~size:40)
      ~book
  in
  print_fill fill;
  print_state simulator ~market_id:(Market_id.of_string "K1");
  [%expect
    {|
    ((filled_size 30) (price 49000000) (fee 60000000) (venue_order_id ()))
    ((cash 470000000) (yes 30) (no 0))
    |}];
  return ()
;;

let%expect_test "polymarket charges no taker fee" =
  let simulator =
    Simulator.create
      ~fill_model:Against_live_book
      ~starting_cash:(Price.of_int_cents 2000)
  in
  let fill =
    Simulator.For_testing.fill_against_book
      simulator
      (order
         ~venue:Polymarket
         ~id:"P1"
         ~contract:No
         ~side:Buy
         ~limit_cents:60
         ~size:5)
      ~book
  in
  print_fill fill;
  [%expect
    {| ((filled_size 5) (price 60000000) (fee 0) (venue_order_id ())) |}];
  return ()
;;

let%expect_test "a limit below the ask rests in reality; the simulator only \
                 models taker fills, so it rejects"
  =
  let simulator =
    Simulator.create
      ~fill_model:Against_live_book
      ~starting_cash:(Price.of_int_cents 2000)
  in
  let fill =
    Simulator.For_testing.fill_against_book
      simulator
      (order
         ~venue:Kalshi
         ~id:"K1"
         ~contract:Yes
         ~side:Buy
         ~limit_cents:45
         ~size:10)
      ~book
  in
  print_fill fill;
  [%expect
    {|
    (rejected
     ("limit is below the ask; the simulator only models taker fills"
      (ask 49000000)
      (order
       ((market
         ((venue Kalshi) (market_id K1) (slug K1) (series_ticker ())
          (clob_token_id ()) (title "test market") (category Crypto)
          (created_time (1970-01-01 00:00:00.000000000Z))
          (close_time (2030-01-01 00:00:00.000000000Z)) (volume ())))
        (contract Yes) (side Buy) (limit_price 45000000) (size 10)))))
    |}];
  return ()
;;

let%expect_test "no depth at the ask is a rejection, and so is any sell — \
                 the book carries no bids"
  =
  let empty_side = { book with yes_ask_size = Size.zero } in
  let simulator =
    Simulator.create
      ~fill_model:Against_live_book
      ~starting_cash:(Price.of_int_cents 2000)
  in
  let fill =
    Simulator.For_testing.fill_against_book
      simulator
      (order
         ~venue:Kalshi
         ~id:"K1"
         ~contract:Yes
         ~side:Buy
         ~limit_cents:52
         ~size:10)
      ~book:empty_side
  in
  print_fill fill;
  let fill =
    Simulator.For_testing.fill_against_book
      simulator
      (order
         ~venue:Kalshi
         ~id:"K1"
         ~contract:Yes
         ~side:Sell
         ~limit_cents:52
         ~size:10)
      ~book
  in
  print_fill fill;
  [%expect
    {|
    (rejected
     ("no depth at the ask"
      (order
       ((market
         ((venue Kalshi) (market_id K1) (slug K1) (series_ticker ())
          (clob_token_id ()) (title "test market") (category Crypto)
          (created_time (1970-01-01 00:00:00.000000000Z))
          (close_time (2030-01-01 00:00:00.000000000Z)) (volume ())))
        (contract Yes) (side Buy) (limit_price 52000000) (size 10)))))
    (rejected
     ("book-aware simulation cannot price sells: Binary_book carries ask depth only"
      (order
       ((market
         ((venue Kalshi) (market_id K1) (slug K1) (series_ticker ())
          (clob_token_id ()) (title "test market") (category Crypto)
          (created_time (1970-01-01 00:00:00.000000000Z))
          (close_time (2030-01-01 00:00:00.000000000Z)) (volume ())))
        (contract Yes) (side Sell) (limit_price 52000000) (size 10)))))
    |}];
  return ()
;;
