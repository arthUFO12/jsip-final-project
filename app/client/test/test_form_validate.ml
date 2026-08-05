(* Tests for {!Client_logic.Form_validate} field messages. *)

open! Core
open Client_logic

let show result = print_s [%sexp (result : (int, string) Result.t)]
let show_float result = print_s [%sexp (result : (float, string) Result.t)]

let%expect_test "int fields parse, bound, and explain" =
  show (Form_validate.lookback_days "14");
  [%expect {| (Ok 14) |}];
  show (Form_validate.lookback_days "45");
  [%expect {| (Error "lookback must be between 1 and 30") |}];
  show (Form_validate.lookback_days "two weeks");
  [%expect {| (Error "lookback must be a whole number") |}];
  show (Form_validate.starting_cash_dollars "-50");
  [%expect {| (Error "starting cash must be between 1 and 1000000") |}];
  show (Form_validate.bot_count "101");
  [%expect {| (Error "number of bots must be between 1 and 100") |}];
  show (Form_validate.fill_price_cents "0");
  [%expect {| (Error "fill price (cents) must be between 1 and 99") |}]
;;

let%expect_test "probability and threshold live on (0,1] and [0,1]" =
  show_float (Form_validate.trade_probability "0.2");
  [%expect {| (Ok 0.2) |}];
  show_float (Form_validate.trade_probability "0");
  [%expect {| (Error "trade probability must be above 0 and at most 1") |}];
  show_float (Form_validate.match_threshold "0");
  [%expect {| (Ok 0) |}];
  show_float (Form_validate.match_threshold "1.5");
  [%expect {| (Error "threshold must be between 0 and 1") |}]
;;

let%expect_test "warmup must fit inside the lookback window" =
  print_s
    [%sexp
      (Form_validate.warmup_within_lookback
         ~warmup_hours:12
         ~lookback_days:14
       : string option)];
  [%expect {| () |}];
  print_s
    [%sexp
      (Form_validate.warmup_within_lookback ~warmup_hours:48 ~lookback_days:2
       : string option)];
  [%expect {| ("warmup must be shorter than the lookback window") |}]
;;

let%expect_test "definition collisions warn before the server would refuse" =
  (* Definitions are written without the [$]; [definition_name] adds it. *)
  let variables = [ "edge = 1 - price(fed_mar)" ] in
  let check draft =
    print_s
      [%sexp
        (Form_validate.definition_collision ~draft ~variables
         : string option)]
  in
  check "edge = 2";
  [%expect {| ("$edge is already defined \226\128\148 pick another name") |}];
  check "cash = 5";
  [%expect {| ("$cash is already defined \226\128\148 pick another name") |}];
  check "fresh = 1";
  [%expect {| () |}];
  check "every 2h buy 1 fed_mar yes";
  [%expect {| () |}]
;;
