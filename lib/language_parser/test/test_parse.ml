(* Tests for {!Parser.Parse}: statements are parsed and then run against
   hand-built environments, printing each rule's qualifiers and the action it
   fires, so the expectations show end behavior rather than AST dumps. *)

open! Core
open Types
open Parser

let slug = Slug.of_string "save-act"

let env ?(cash = 100.) ?(price = 0.40) ?(inventory = 0.) () : Eval_env.t =
  { cash
  ; realized = 0.
  ; unrealized = 0.
  ; price_ago = (fun ~slug:_ ~contract:_ ~ago:_ -> Some price)
  ; inventory = (fun ~slug:_ -> inventory)
  }
;;

let show ?(env = env ()) text =
  match Parse.program text ~slugs:[ slug ] with
  | Error error -> print_s [%sexp (error : Error.t)]
  | Ok rules ->
    let eval = Expr.Eval.create env in
    List.iter rules ~f:(fun rule ->
      let every =
        match rule.every with
        | None -> ""
        | Some span -> [%string "every %{span#Time_ns.Span} "]
      in
      let when_i =
        match rule.when_i with
        | None -> ""
        | Some { side = Buy; slug } -> [%string "when-i-buy %{slug#Slug} "]
        | Some { side = Sell; slug } -> [%string "when-i-sell %{slug#Slug} "]
      in
      let fired =
        match Rule.eval rule eval with
        | None -> "no action"
        | Some { side; size; slug; contract } ->
          let side = match side with Buy -> "buy" | Sell -> "sell" in
          let contract =
            match contract with Contract_type.Yes -> "yes" | No -> "no"
          in
          let size = Expr.Eval.num eval size in
          [%string "%{side} %{size#Float} %{contract} %{slug#Slug}"]
      in
      print_endline (every ^ when_i ^ fired))
;;

let%expect_test "bare actions with qualifiers" =
  show "EVERY 2h BUY 5 save-act YES";
  [%expect {| every 2h buy 5. yes save-act |}];
  show "WHEN I BUY save-act SELL 3 save-act NO";
  [%expect {| when-i-buy save-act sell 3. no save-act |}];
  show "EVERY 1d WHEN I SELL save-act BUY 1 save-act YES";
  [%expect {| every 1d when-i-sell save-act buy 1. yes save-act |}]
;;

let%expect_test "if/then/else picks a branch from the condition" =
  let program =
    "IF $cash > 50 THEN BUY 2 save-act YES ELSE SELL 2 save-act YES"
  in
  show program ~env:(env ~cash:100. ());
  [%expect {| buy 2. yes save-act |}];
  show program ~env:(env ~cash:10. ());
  [%expect {| sell 2. yes save-act |}];
  show "IF $cash > 50 THEN BUY 2 save-act YES" ~env:(env ~cash:10. ());
  [%expect {| no action |}]
;;

let%expect_test "variables define in order, and sizes are expressions" =
  show
    {|lot = 2 + 3
      double_lot = $lot * 2
      EVERY 1h BUY $double_lot - 1 save-act YES|};
  [%expect {| every 1h buy 9. yes save-act |}];
  show
    {|cheap = save-act < 0.50
      funded = $cash >= 50
      IF $cheap && $funded THEN BUY 1 save-act YES|};
  [%expect {| buy 1. yes save-act |}]
;;

let%expect_test "market references: bare ticker, PRICE, and INVENTORY" =
  (* The env's price is 0.40 throughout. *)
  show "IF save-act < 0.50 THEN BUY 1 save-act YES";
  [%expect {| buy 1. yes save-act |}];
  show "IF PRICE save-act == 0.40 THEN BUY 1 save-act YES";
  [%expect {| buy 1. yes save-act |}];
  (* The NO price is written arithmetically. *)
  show "IF 1 - save-act > 0.55 THEN BUY 1 save-act NO";
  [%expect {| buy 1. no save-act |}];
  (* Sizes can read the position: top the holding up to 10 contracts. *)
  show
    ~env:(env ~inventory:4. ())
    "EVERY 1h BUY 10 - INVENTORY save-act save-act YES";
  [%expect {| every 1h buy 6. yes save-act |}];
  show
    ~env:(env ~inventory:4. ())
    "IF INVENTORY save-act < 3 THEN BUY 1 save-act YES";
  [%expect {| no action |}]
;;

let%expect_test "signal statements parse both magnitudes and windows" =
  (* Price is flat in this env, so only a 0-move threshold fires. *)
  show "IF save-act YES UP BY 0 SINCE 2h AGO THEN BUY 1 save-act YES";
  [%expect {| buy 1. yes save-act |}];
  show "IF save-act YES UP BY 5% SINCE 2h AGO THEN BUY 1 save-act YES";
  [%expect {| no action |}];
  show
    "IF save-act NO DOWN BY 0.10 SINCE 1d AGO END 2h AGO THEN SELL 1 \
     save-act NO";
  [%expect {| no action |}]
;;

let%expect_test "operator precedence: || ^ && ! and arithmetic" =
  (* && binds tighter than ||: true || (false && false). *)
  show "IF true || false && false THEN BUY 1 save-act YES";
  [%expect {| buy 1. yes save-act |}];
  (* ! binds tighter than ^: (!true) ^ true. *)
  show "IF !true ^ true THEN BUY 1 save-act YES";
  [%expect {| buy 1. yes save-act |}];
  (* * over +, and parens override. *)
  show "IF 2 + 3 * 4 == 14 THEN BUY 1 save-act YES";
  [%expect {| buy 1. yes save-act |}];
  show "IF (2 + 3) * 4 == 20 THEN BUY 1 save-act YES";
  [%expect {| buy 1. yes save-act |}]
;;

let%expect_test "parse errors carry the line and reason" =
  show "BUY 5 save-act YES";
  [%expect
    {|
    ("parse error" (line 1) (line "BUY 5 save-act YES")
     "a bare action statement requires an EVERY or WHEN I qualifier")
    |}];
  show "EVERY 2h BUY $mystery save-act YES";
  [%expect
    {|
    ("parse error" (line 1) (line "EVERY 2h BUY $mystery save-act YES")
     ("unknown variable" (name mystery)))
    |}];
  show {|lot = 5
      lot = 6|};
  [%expect
    {|
    ("parse error" (line 2) (line "lot = 6")
     ("variable is already defined" (name lot)))
    |}];
  show {|flag = true
      EVERY 1h BUY $flag save-act YES|};
  [%expect
    {|
    ("parse error" (line 2) (line "EVERY 1h BUY $flag save-act YES")
     ("variable used where a numeric value is required" (name flag)
      (defined_as boolean)))
    |}];
  show "EVERY 2h IF $cash THEN BUY 1 save-act YES";
  [%expect
    {|
    ("parse error" (line 1) (line "EVERY 2h IF $cash THEN BUY 1 save-act YES")
     ("variable used where a boolean value is required" (name cash)
      (defined_as numeric)))
    |}];
  show "IF PRICE moon > 0 THEN BUY 1 save-act YES";
  [%expect
    {|
    ("parse error" (line 1) (line "IF PRICE moon > 0 THEN BUY 1 save-act YES")
     "unknown market 'moon'")
    |}];
  show "IF PRICE save-act THEN BUY 1 save-act YES";
  [%expect
    {|
    ("parse error" (line 1) (line "IF PRICE save-act THEN BUY 1 save-act YES")
     "price is a numeric value; compare it to something to form a condition")
    |}];
  show "save-act = 5";
  [%expect
    {|
    ("parse error" (line 1) (line "save-act = 5")
     "variable name 'save-act' collides with a market ticker")
    |}];
  show "IF save-act YES SIDEWAYS BY 3% SINCE 1h AGO THEN BUY 1 save-act YES";
  [%expect
    {|
    ("parse error" (line 1)
     (line "IF save-act YES SIDEWAYS BY 3% SINCE 1h AGO THEN BUY 1 save-act YES")
     "expected up or down, found 'SIDEWAYS'")
    |}];
  (* Errors are collected across lines, not just the first. *)
  show {|EVERY 2h BUY $a save-act YES
      EVERY 2h BUY $b save-act YES|};
  [%expect
    {|
    (("parse error" (line 1) (line "EVERY 2h BUY $a save-act YES")
      ("unknown variable" (name a)))
     ("parse error" (line 2) (line "EVERY 2h BUY $b save-act YES")
      ("unknown variable" (name b))))
    |}]
;;

let%expect_test "parsed spans and windows validate through Program.create" =
  let compile text =
    let open Or_error.Let_syntax in
    let%bind rules = Parse.program text ~slugs:[ slug ] in
    let%bind (_ : Program.t) =
      Program.create ~rules ~tick:(Time_ns.Span.of_hr 1.) ~slugs:[ slug ]
    in
    return ()
  in
  print_s [%sexp (compile "EVERY 2h BUY 1 save-act YES" : unit Or_error.t)];
  [%expect {| (Ok ()) |}];
  print_s [%sexp (compile "EVERY 90m BUY 1 save-act YES" : unit Or_error.t)];
  [%expect
    {|
    (Error
     ("span is not a whole number of simulation ticks" (what "EVERY interval")
      (span 1h30m) (tick 1h)))
    |}];
  print_s
    [%sexp (compile "EVERY 1h BUY 1 other-market YES" : unit Or_error.t)];
  [%expect
    {|
    (Error
     ("rule references a market not in this simulation" (slug other-market)))
    |}]
;;

let%expect_test "lowercase keywords and normalized tickers resolve to the \
                 original slug"
  =
  let kalshi = Slug.of_string "KXELONMARS--99" in
  let show text =
    match Parse.program text ~slugs:[ kalshi ] with
    | Error error -> print_s [%sexp (error : Error.t)]
    | Ok rules ->
      let eval = Expr.Eval.create (env ~price:0.60 ()) in
      List.iter rules ~f:(fun rule ->
        match Rule.eval rule eval with
        | None -> print_endline "no action"
        | Some { side = _; size = _; slug; contract = _ } ->
          print_endline [%string "fires on %{slug#Slug}"])
  in
  (* The venue slug is uppercase with a double hyphen; the program spells it
     lowercase with double underscores and gets the real slug back. *)
  show "every 2h if price kxelonmars__99 > 0.5 then buy 1 kxelonmars__99 yes";
  [%expect {| fires on KXELONMARS--99 |}];
  (* Mixed casing of keywords is fine too. *)
  show "EVERY 2h Buy 1 kxelonmars__99 YES";
  [%expect {| fires on KXELONMARS--99 |}]
;;

let%expect_test "ticker vocabularies that cannot normalize are \
                 whole-program errors"
  =
  let show slugs =
    match Parse.program "every 2h buy 1 x yes" ~slugs with
    | Error error -> print_s [%sexp (error : Error.t)]
    | Ok (_ : Rule.t list) -> print_endline "parsed"
  in
  show [ Slug.of_string "KX-A"; Slug.of_string "kx_a" ];
  [%expect
    {|
    ("two market tickers normalize to the same name" (name kx_a) (slug kx_a)
     (clashes_with KX-A))
    |}];
  show [ Slug.of_string "BUY" ];
  [%expect
    {| ("market ticker collides with a language keyword" (slug BUY) (keyword buy)) |}]
;;
