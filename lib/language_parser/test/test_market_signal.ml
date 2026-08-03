(* Tests for {!Parser.Market_signal}. *)

open! Core
open Types
open Parser

let slug = Slug.of_string "save-act"

let env_with_price price : Eval_env.t =
  { cash = 0.
  ; realized = 0.
  ; unrealized = 0.
  ; price_ago = (fun ~slug:_ ~contract:_ ~ago:_ -> price)
  ; inventory = (fun ~slug:_ -> 0.)
  }
;;

let%expect_test "above and below are strict threshold checks" =
  let check signal =
    List.map [ 0.30; 0.40; 0.50 ] ~f:(fun price ->
      Market_signal.evaluate signal ~env:(env_with_price (Some price)))
  in
  let above = check (Above { slug; contract = Yes; threshold = 0.40 }) in
  let below = check (Below { slug; contract = Yes; threshold = 0.40 }) in
  print_s [%message "" (above : bool list) (below : bool list)];
  [%expect {| ((above (false false true)) (below (true false false))) |}]
;;

let%expect_test "missing history evaluates to false" =
  let env = env_with_price None in
  let above : Market_signal.t =
    Above { slug; contract = Yes; threshold = 0.40 }
  in
  let below : Market_signal.t =
    Below { slug; contract = Yes; threshold = 0.40 }
  in
  print_s
    [%message
      ""
        ~above:(Market_signal.evaluate above ~env : bool)
        ~below:(Market_signal.evaluate below ~env : bool)];
  [%expect {| ((above false) (below false)) |}]
;;

(* [history] maps whole hours ago -> yes price; [None] beyond its reach. *)
let env_with_history history : Eval_env.t =
  { cash = 0.
  ; realized = 0.
  ; unrealized = 0.
  ; price_ago =
      (fun ~slug:_ ~contract:_ ~ago ->
        let hours_ago =
          Time_ns.Span.to_int_ns ago
          / Time_ns.Span.to_int_ns (Time_ns.Span.of_hr 1.)
        in
        List.Assoc.find history hours_ago ~equal:Int.equal)
  ; inventory = (fun ~slug:_ -> 0.)
  }
;;

let moved ~direction ~by ~since ?end_ ~env () =
  let window : Market_signal.Window.t =
    { start_ago = Time_ns.Span.of_hr since
    ; end_ago =
        (match end_ with
         | None -> Time_ns.Span.zero
         | Some hours -> Time_ns.Span.of_hr hours)
    }
  in
  Market_signal.evaluate
    (Moved { slug; contract = Yes; direction; window; by })
    ~env
;;

let%expect_test "moved by amount is an at-least check in the stated \
                 direction"
  =
  (* 0.40 three hours ago, 0.50 now: up exactly $0.10. *)
  let env = env_with_history [ 3, 0.40; 0, 0.50 ] in
  let up amount =
    moved ~direction:Up ~by:(Amount amount) ~since:3. ~env ()
  in
  let down amount =
    moved ~direction:Down ~by:(Amount amount) ~since:3. ~env ()
  in
  print_s
    [%message
      ""
        ~up_at_boundary:(up 0.10 : bool)
        ~up_beyond:(up 0.11 : bool)
        ~down:(down 0.05 : bool)];
  [%expect {| ((up_at_boundary true) (up_beyond false) (down false)) |}]
;;

let%expect_test "moved by percent measures against the start-of-window price"
  =
  (* 0.40 -> 0.50 is a 25% rise on the start price. *)
  let env = env_with_history [ 3, 0.40; 0, 0.50 ] in
  let up pct = moved ~direction:Up ~by:(Percent pct) ~since:3. ~env () in
  print_s [%message "" ~at_boundary:(up 25. : bool) ~beyond:(up 26. : bool)];
  [%expect {| ((at_boundary true) (beyond false)) |}];
  (* A zero start price has no defined percent change. *)
  let env = env_with_history [ 3, 0.; 0, 0.50 ] in
  print_s
    [%sexp (moved ~direction:Up ~by:(Percent 1.) ~since:3. ~env () : bool)];
  [%expect {| false |}]
;;

let%expect_test "an END offset measures a window that closed in the past" =
  (* Rose 0.40 -> 0.60 between 4h and 2h ago, then fell to 0.30. *)
  let env = env_with_history [ 4, 0.40; 2, 0.60; 0, 0.30 ] in
  let up ?end_ () =
    moved ~direction:Up ~by:(Amount 0.15) ~since:4. ?end_ ~env ()
  in
  print_s
    [%message
      "" ~whole_window:(up () : bool) ~closed_early:(up ~end_:2. () : bool)];
  [%expect {| ((whole_window false) (closed_early true)) |}]
;;

let%expect_test "moved with missing history evaluates to false" =
  let env = env_with_history [ 3, 0.40; 0, 0.50 ] in
  print_s
    [%sexp (moved ~direction:Up ~by:(Amount 0.01) ~since:5. ~env () : bool)];
  [%expect {| false |}]
;;
