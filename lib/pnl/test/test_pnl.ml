open! Core
open Types

(* All tests trade one market. Prices are set with a consistent complement
   book: no_bid = $1 - yes_ask and no_ask = $1 - yes_bid, so the yes and no
   sides agree and the spread is visible in both. *)

let slug = Slug.of_string "will-it-rain"
let cents = Price.of_int_cents

let set_prices t ~yes_bid ~yes_ask =
  let yes_bbo : Bbo.t = { bid = cents yes_bid; ask = cents yes_ask } in
  let no_bbo : Bbo.t =
    { bid = cents (100 - yes_ask); ask = cents (100 - yes_bid) }
  in
  Pnl.apply_trade_report ~slug ~yes_bbo ~no_bbo t
;;

let make_pnl ~yes_bid ~yes_ask =
  let t = Pnl.create [ slug ] Price.zero in
  set_prices t ~yes_bid ~yes_ask;
  t
;;

let show t =
  let cash = Price.to_string_dollar (Pnl.cash t) in
  let realized = Price.to_string_dollar (Pnl.realized_pnl t) in
  let unrealized = Price.to_string_dollar (Pnl.unrealized_pnl t) in
  let avg_cost =
    Price.to_string_dollar (Hashtbl.find_exn (Pnl.average_cost_bases t) slug)
  in
  print_endline
    [%string
      "cash %{cash} | realized %{realized} | unrealized %{unrealized} | avg \
       cost %{avg_cost}"]
;;

let%expect_test "buy yes, mark up, sell at a profit: cash moves" =
  let t = make_pnl ~yes_bid:38 ~yes_ask:40 in
  Pnl.update_position
    t
    ~slug
    ~side:Buy
    ~contract_type:Yes
    ~size:(Size.of_int 10);
  show t;
  (* Paid 10 x $0.40 ask; marking at the $0.38 bid costs the spread. *)
  [%expect
    {| cash -$4.00 | realized $0.00 | unrealized -$0.20 | avg cost $0.40 |}];
  set_prices t ~yes_bid:60 ~yes_ask:62;
  show t;
  [%expect
    {| cash -$4.00 | realized $0.00 | unrealized $2.00 | avg cost $0.40 |}];
  Pnl.update_position
    t
    ~slug
    ~side:Sell
    ~contract_type:Yes
    ~size:(Size.of_int 10);
  show t;
  (* Sale proceeds (10 x $0.60 bid) land in cash, not just cost basis. *)
  [%expect
    {| cash $2.00 | realized $2.00 | unrealized $0.00 | avg cost $0.00 |}]
;;

let%expect_test "no positions mark at the no bid, not upside down" =
  let t = make_pnl ~yes_bid:38 ~yes_ask:40 in
  Pnl.update_position
    t
    ~slug
    ~side:Buy
    ~contract_type:No
    ~size:(Size.of_int 5);
  show t;
  (* Paid 5 x $0.62 no ask; marked at the $0.60 no bid. *)
  [%expect
    {| cash -$3.10 | realized $0.00 | unrealized -$0.10 | avg cost $0.62 |}];
  set_prices t ~yes_bid:18 ~yes_ask:20;
  show t;
  (* Yes falls to 18/20, so the no bid is $0.80 and the position gains. *)
  [%expect
    {| cash -$3.10 | realized $0.00 | unrealized $0.90 | avg cost $0.62 |}]
;;

let day_after date = Time_ns.add date (Time_ns.Span.of_day 1.)

let%expect_test "winning no position is paid $1 per contract at expiry" =
  let t = make_pnl ~yes_bid:38 ~yes_ask:40 in
  Pnl.update_position
    t
    ~slug
    ~side:Buy
    ~contract_type:No
    ~size:(Size.of_int 5);
  Pnl.set_expiration t ~slug ~date:Time_ns.epoch ~winner:No;
  Pnl.settle_expired_bets t ~now:(day_after Time_ns.epoch);
  show t;
  (* Paid $3.10 for 5 no contracts, collected 5 x $1.00. *)
  [%expect
    {| cash $1.90 | realized $1.90 | unrealized $0.00 | avg cost $0.00 |}];
  (* Settling again must not pay out twice. *)
  Pnl.settle_expired_bets t ~now:(day_after Time_ns.epoch);
  show t;
  [%expect
    {| cash $1.90 | realized $1.90 | unrealized $0.00 | avg cost $0.00 |}]
;;

let%expect_test "losing yes position is written off at expiry" =
  let t = make_pnl ~yes_bid:38 ~yes_ask:40 in
  Pnl.update_position
    t
    ~slug
    ~side:Buy
    ~contract_type:Yes
    ~size:(Size.of_int 10);
  Pnl.set_expiration t ~slug ~date:Time_ns.epoch ~winner:No;
  Pnl.settle_expired_bets t ~now:(day_after Time_ns.epoch);
  show t;
  [%expect
    {| cash -$4.00 | realized -$4.00 | unrealized $0.00 | avg cost $0.00 |}]
;;

let%expect_test "flipping from yes to no through zero" =
  let t = make_pnl ~yes_bid:60 ~yes_ask:62 in
  Pnl.update_position
    t
    ~slug
    ~side:Buy
    ~contract_type:Yes
    ~size:(Size.of_int 2);
  Pnl.update_position
    t
    ~slug
    ~side:Sell
    ~contract_type:Yes
    ~size:(Size.of_int 5);
  show t;
  [%expect
    {| cash -$1.24 | realized -$0.04 | unrealized -$0.06 | avg cost $0.40 |}]
;;

let%expect_test "average cost basis tracks entry prices through the trades" =
  let t = make_pnl ~yes_bid:38 ~yes_ask:40 in
  Pnl.update_position
    t
    ~slug
    ~side:Buy
    ~contract_type:Yes
    ~size:(Size.of_int 10);
  (* 10 bought at the $0.40 ask. *)
  show t;
  [%expect
    {| cash -$4.00 | realized $0.00 | unrealized -$0.20 | avg cost $0.40 |}];
  set_prices t ~yes_bid:58 ~yes_ask:60;
  Pnl.update_position
    t
    ~slug
    ~side:Buy
    ~contract_type:Yes
    ~size:(Size.of_int 10);
  (* 10 more at $0.60: the average blends to $0.50. *)
  show t;
  [%expect
    {| cash -$10.00 | realized $0.00 | unrealized $1.60 | avg cost $0.50 |}];
  Pnl.update_position
    t
    ~slug
    ~side:Sell
    ~contract_type:Yes
    ~size:(Size.of_int 15);
  (* Reducing removes cost proportionally, so the average is unchanged. *)
  show t;
  [%expect
    {| cash -$1.30 | realized $1.20 | unrealized $0.40 | avg cost $0.50 |}];
  Pnl.update_position
    t
    ~slug
    ~side:Sell
    ~contract_type:Yes
    ~size:(Size.of_int 10);
  (* Flip through flat: a short-yes position is a long-no position, so the
     5-contract short's basis is the $0.42 no ask (1 - the $0.58 bid). *)
  show t;
  [%expect
    {| cash -$0.50 | realized $1.60 | unrealized -$0.10 | avg cost $0.42 |}]
;;

let show_effect t ~side ~contract_type ~size =
  let { Pnl.Trade_effect.cash_after; enlarges_position } =
    Pnl.trade_effect t ~slug ~side ~contract_type ~size:(Size.of_int size)
  in
  let cash_after = Price.to_string_dollar cash_after in
  print_endline
    [%string "cash after %{cash_after} | enlarges %{enlarges_position#Bool}"]
;;

let%expect_test "trade_effect previews without applying" =
  let t = make_pnl ~yes_bid:38 ~yes_ask:40 in
  (* An opening buy is enlarging and spends cash... *)
  show_effect t ~side:Buy ~contract_type:Yes ~size:10;
  [%expect {| cash after -$4.00 | enlarges true |}];
  (* ...but the preview left the book untouched. *)
  show t;
  [%expect
    {| cash $0.00 | realized $0.00 | unrealized $0.00 | avg cost $0.00 |}];
  Pnl.update_position
    t
    ~slug
    ~side:Buy
    ~contract_type:Yes
    ~size:(Size.of_int 10);
  (* Reducing trades are never enlarging, even while cash is negative. *)
  show_effect t ~side:Sell ~contract_type:Yes ~size:4;
  [%expect {| cash after -$2.48 | enlarges false |}];
  (* A flip past flat opens a bigger short than it closes: enlarging. *)
  show_effect t ~side:Sell ~contract_type:Yes ~size:25;
  [%expect {| cash after -$9.50 | enlarges true |}];
  (* Zero-size trades change nothing. *)
  show_effect t ~side:Buy ~contract_type:Yes ~size:0;
  [%expect {| cash after -$4.00 | enlarges false |}]
;;
