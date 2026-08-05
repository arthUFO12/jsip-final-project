(* Tests for {!Client_logic.Rule_candidates}: the editor env's Var_env-like
   growth and the end-of-buffer token editing helpers. (Suggestion ranking
   itself is {!Autocomplete}'s concern, tested in lib/autocomplete.) *)

open! Core
open Types
open Client_logic

let tickers = [ Slug.of_string "save-act" ]

let%expect_test "definitions in the buffer become $variable candidates" =
  let program =
    {|lot = 2 + 3
cheap = save-act < 0.5
IF $cheap THEN BUY $lot save-act YES|}
  in
  let env = Rule_candidates.env ~tickers ~variables:[] ~program in
  Autocomplete.candidates env
  |> List.filter ~f:(String.is_prefix ~prefix:"$")
  |> List.iter ~f:print_endline;
  [%expect
    {|
    $cash
    $realized
    $unrealized
    $lot
    $cheap
    |}]
;;

let%expect_test "comparison lines and nonsense do not define variables" =
  let program = {|IF $cash == 100 THEN BUY 1 save-act YES
2 + 2 = 4|} in
  let env = Rule_candidates.env ~tickers ~variables:[] ~program in
  Autocomplete.candidates env
  |> List.filter ~f:(String.is_prefix ~prefix:"$")
  |> List.iter ~f:print_endline;
  [%expect {|
    $cash
    $realized
    $unrealized
    |}]
;;

let%expect_test "panel variables become $candidates without touching the \
                 buffer"
  =
  let env =
    Rule_candidates.env
      ~tickers
      ~variables:[ "lot = 2 + 3"; "not a definition" ]
      ~program:""
  in
  Autocomplete.candidates env
  |> List.filter ~f:(String.is_prefix ~prefix:"$")
  |> List.iter ~f:print_endline;
  [%expect {|
    $cash
    $realized
    $unrealized
    $lot
    |}]
;;

let%expect_test "definition_name extracts the $name from a definition line" =
  List.iter
    [ "lot = 2 + 3"; "x=1"; "2 + 2 = 4"; "a == b"; "no equals here" ]
    ~f:(fun line ->
      print_s [%sexp (Rule_candidates.definition_name line : string option)]);
  [%expect {|
    ($lot)
    ($x)
    ()
    ()
    ()
    |}]
;;

let%expect_test "tickers are offered in their normalized program spelling" =
  let tickers =
    List.map [ "KXELONMARS-99"; "KXALBUM--26OCT01" ] ~f:Slug.of_string
  in
  let env = Rule_candidates.env ~tickers ~variables:[] ~program:"" in
  Autocomplete.suggest env ~input:"kx" |> List.iter ~f:print_endline;
  [%expect {|
    kxalbum__26oct01
    kxelonmars_99
    |}]
;;

let%expect_test "current_token grabs the trailing word only" =
  List.iter
    [ "EVERY 2h BUY 1 save"; "IF $ca"; "IF save-act > 0.3 "; "" ]
    ~f:(fun text ->
      print_endline [%string "<%{Rule_candidates.current_token text}>"]);
  [%expect {|
    <save>
    <$ca>
    <>
    <>
    |}]
;;

let%expect_test "complete replaces the trailing token" =
  print_endline
    (Rule_candidates.complete "EVERY 2h BUY 1 save" ~with_:"save-act");
  [%expect {| EVERY 2h BUY 1 save-act |}];
  print_endline (Rule_candidates.complete "IF $ca" ~with_:"$cash");
  [%expect {| IF $cash |}]
;;
