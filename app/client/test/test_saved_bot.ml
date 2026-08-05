(* Tests for {!Client_logic.Saved_bot}: storage round-trip, tolerant parsing,
   and the upsert/find/remove list operations behind the saved-bots dropdown. *)

open! Core
open Types
open Client_logic

let bot ?(rules = [ "every 2h buy 1 save_act yes" ]) name : Saved_bot.t =
  { name
  ; slugs = [ Slug.of_string "SAVE-ACT" ]
  ; rules
  ; variables = [ "lot = 2" ]
  ; interval = "hour"
  ; lookback = "14"
  ; warmup = "12"
  ; starting_cash = "100"
  ; allow_negative = false
  }
;;

let show bots =
  List.iter bots ~f:(fun (bot : Saved_bot.t) ->
    print_endline
      [%string "%{bot.name}: %{String.concat bot.rules ~sep:\"; \"}"])
;;

let%expect_test "storage round-trips exactly" =
  let bots =
    [ bot "alpha"
    ; bot "beta" ~rules:[ "if $cash > 50 then buy 1 save_act yes" ]
    ]
  in
  let restored = Saved_bot.(list_of_string (list_to_string bots)) in
  print_s [%message (List.equal Saved_bot.equal bots restored : bool)];
  [%expect {| ("List.equal Saved_bot.equal bots restored" true) |}]
;;

let%expect_test "garbage in storage reads as no saved bots" =
  show (Saved_bot.list_of_string "not a sexp (");
  show (Saved_bot.list_of_string "");
  [%expect {| |}]
;;

let%expect_test "upsert adds new names; re-saving moves a bot to the end" =
  let bots = Saved_bot.upsert [] (bot "alpha") in
  let bots = Saved_bot.upsert bots (bot "beta") in
  show bots;
  [%expect
    {|
    alpha: every 2h buy 1 save_act yes
    beta: every 2h buy 1 save_act yes
    |}];
  let bots =
    Saved_bot.upsert bots (bot "alpha" ~rules:[ "sell 1 save_act no" ])
  in
  show bots;
  [%expect
    {|
    alpha: sell 1 save_act no
    beta: every 2h buy 1 save_act yes
    |}]
;;

let%expect_test "find and remove work by name" =
  let bots = [ bot "alpha"; bot "beta" ] in
  print_s
    [%sexp
      (Option.map (Saved_bot.find bots ~name:"beta") ~f:(fun bot -> bot.name)
       : string option)];
  print_s [%sexp (Option.is_none (Saved_bot.find bots ~name:"gamma") : bool)];
  show (Saved_bot.remove bots ~name:"alpha");
  [%expect
    {|
    (beta)
    true
    beta: every 2h buy 1 save_act yes
    |}]
;;
