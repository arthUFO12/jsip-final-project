(* Tests for {!Autocomplete}: env growth and suggestion ranking. *)

open! Core

let rule_env =
  Autocomplete.of_list
    [ "IF"; "THEN"; "INVENTORY"; "SINCE"; "save-act"; "$cash" ]
;;

let show suggestions = print_s [%sexp (suggestions : string list)]

let%expect_test "prefix matches beat substring matches" =
  (* "in" is a prefix of INVENTORY but only inside SINCE. *)
  show (Autocomplete.suggest rule_env ~input:"in");
  [%expect {| (INVENTORY SINCE) |}]
;;

let%expect_test "ties break alphabetically" =
  let env = Autocomplete.of_list [ "sell-b"; "sell-a"; "unsellable" ] in
  show (Autocomplete.suggest env ~input:"sell");
  [%expect {| (sell-a sell-b unsellable) |}]
;;

let%expect_test "exact and empty inputs suggest nothing" =
  show (Autocomplete.suggest rule_env ~input:"IF");
  [%expect {| () |}];
  show (Autocomplete.suggest rule_env ~input:"   ");
  [%expect {| () |}]
;;

let%expect_test "matching is case-insensitive, spelling is preserved" =
  show (Autocomplete.suggest rule_env ~input:"inv");
  [%expect {| (INVENTORY) |}];
  show (Autocomplete.suggest rule_env ~input:"$c");
  [%expect {| ($cash) |}]
;;

let%expect_test "the env grows like Var_env and dedupes caselessly" =
  let env = Autocomplete.add rule_env "$lot" in
  let env = Autocomplete.add env "$LOT" in
  show (Autocomplete.suggest env ~input:"$l");
  [%expect {| ($lot) |}]
;;

let%expect_test "limit caps the suggestion list" =
  let env =
    Autocomplete.of_list
      (List.init 20 ~f:(fun i -> [%string "market-%{i#Int}"]))
  in
  show (Autocomplete.suggest env ~input:"market" ~limit:3);
  [%expect {| (market-0 market-1 market-10) |}]
;;

let%expect_test "uppercase prefix matches still beat substring matches" =
  (* "final" contains "in"; INVENTORY starts with it (case-insensitively).
     Alphabetical order alone would put "final" first — tier must win. *)
  let env = Autocomplete.of_list [ "final"; "INVENTORY" ] in
  show (Autocomplete.suggest env ~input:"in");
  [%expect {| (INVENTORY final) |}]
;;
