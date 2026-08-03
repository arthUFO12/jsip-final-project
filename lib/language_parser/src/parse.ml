open! Core
open Types

(* Internal control flow only: every raise is caught at the statement
   boundary and converted to the [Or_error.t] that [program] returns. The
   payload is a sexp so nested errors ([Var_env] lookups, [Rule.create])
   print structurally instead of as escaped strings. *)
exception Parse_error of Sexp.t

let fail message = raise (Parse_error (Sexp.Atom message))

let or_fail = function
  | Ok value -> value
  | Error error -> raise (Parse_error (Error.sexp_of_t error))
;;

(* A statement's parse state. [vars] grows as variable definitions are parsed
   and is committed back to the caller only if the whole statement parses, so
   failed statements leave no bindings behind. *)
type state =
  { tokens : Token.t array
  ; mutable pos : int
  ; ctx : Expr.Context.t
  ; mutable vars : Var_env.t
  ; slug_map : Slug.t String.Map.t
  (** {!Ticker_name.normalize}d ticker -> real slug for the simulation's
      markets: a bare word is a price reference exactly when its normalized
      form is a key. *)
  }

let peek state =
  match state.pos < Array.length state.tokens with
  | true -> Some state.tokens.(state.pos)
  | false -> None
;;

let advance state = state.pos <- state.pos + 1

let next state ~expecting =
  match peek state with
  | Some token ->
    advance state;
    token
  | None -> fail [%string "expected %{expecting} but the line ended"]
;;

let expect state token ~expecting =
  let found = next state ~expecting in
  match Token.equal token found with
  | true -> ()
  | false ->
    fail
      [%string "expected %{expecting} but found '%{Token.to_string found}'"]
;;

(* Keywords are canonically lowercase but accepted in any casing. *)
let expect_word state word =
  match next state ~expecting:word with
  | Word found when String.Caseless.equal found word -> ()
  | token ->
    fail [%string "expected %{word} but found '%{Token.to_string token}'"]
;;

let at_word state word =
  match peek state with
  | Some (Word found) -> String.Caseless.equal found word
  | Some
      ( Number _ | Percent_lit _ | Duration _ | Var _ | Lparen | Rparen
      | Assign | Eq | Ne | Ge | Le | Gt | Lt | And_op | Or_op | Xor_op
      | Not_op | Plus | Minus | Star | Slash )
  | None ->
    false
;;

(* Backtracking point for the ambiguous corners of the grammar (is '(' a
   numeric or a boolean parenthesis?). *)
let try_parse state ~f =
  let saved = state.pos in
  match f state with
  | value -> Some value
  | exception Parse_error _ ->
    state.pos <- saved;
    None
;;

let find_slug state word =
  Map.find state.slug_map (Ticker_name.normalize word)
;;

(* Slug positions that are validated later (by {!Rule.create} /
   [Program.create]) keep unknown words as-is so those errors still name what
   the user typed. *)
let slug_or_raw state word =
  match find_slug state word with
  | Some slug -> slug
  | None -> Slug.of_string word
;;

let parse_slug state =
  match next state ~expecting:"a market slug" with
  | Word word -> slug_or_raw state word
  | token ->
    fail [%string "expected a market slug, found '%{Token.to_string token}'"]
;;

let parse_contract state =
  match next state ~expecting:"yes or no" with
  | Word word when String.Caseless.equal word "yes" -> Contract_type.Yes
  | Word word when String.Caseless.equal word "no" -> Contract_type.No
  | token ->
    fail [%string "expected yes or no, found '%{Token.to_string token}'"]
;;

let parse_side state =
  match next state ~expecting:"buy or sell" with
  | Word word when String.Caseless.equal word "buy" -> Side.Buy
  | Word word when String.Caseless.equal word "sell" -> Side.Sell
  | token ->
    fail [%string "expected buy or sell, found '%{Token.to_string token}'"]
;;

(* [price]/[inventory] name their market explicitly, so an unknown ticker
   there is an error; bare-ticker price references are recognized by
   membership alone in {!parse_num_factor}. *)
let parse_known_slug state ~keyword =
  let expecting = [%string "a market ticker after %{keyword}"] in
  match next state ~expecting with
  | Word word ->
    (match find_slug state word with
     | Some slug -> slug
     | None -> fail [%string "unknown market '%{word}'"])
  | token ->
    fail [%string "expected %{expecting}, found '%{Token.to_string token}'"]
;;

let parse_duration state ~expecting =
  match next state ~expecting with
  | Duration span -> span
  | token ->
    fail
      [%string
        "expected %{expecting} like 3h or 30m, found '%{Token.to_string \
         token}'"]
;;

let rec parse_num_expr state =
  let lhs = parse_num_term state in
  let rec go lhs =
    match peek state with
    | Some Plus ->
      advance state;
      go (Expr.Num.add state.ctx lhs (parse_num_term state))
    | Some Minus ->
      advance state;
      go (Expr.Num.sub state.ctx lhs (parse_num_term state))
    | Some
        ( Number _ | Percent_lit _ | Duration _ | Word _ | Var _ | Lparen
        | Rparen | Assign | Eq | Ne | Ge | Le | Gt | Lt | And_op | Or_op
        | Xor_op | Not_op | Star | Slash )
    | None ->
      lhs
  in
  go lhs

and parse_num_term state =
  let lhs = parse_num_factor state in
  let rec go lhs =
    match peek state with
    | Some Star ->
      advance state;
      go (Expr.Num.mul state.ctx lhs (parse_num_factor state))
    | Some Slash ->
      advance state;
      go (Expr.Num.div state.ctx lhs (parse_num_factor state))
    | Some
        ( Number _ | Percent_lit _ | Duration _ | Word _ | Var _ | Lparen
        | Rparen | Assign | Eq | Ne | Ge | Le | Gt | Lt | And_op | Or_op
        | Xor_op | Not_op | Plus | Minus )
    | None ->
      lhs
  in
  go lhs

and parse_num_factor state =
  match next state ~expecting:"a number, $variable, ticker, or '('" with
  | Number value -> Expr.Num.const state.ctx value
  | Minus -> Expr.Num.neg state.ctx (parse_num_factor state)
  | Var name -> or_fail (Var_env.find_num state.vars name)
  | Word word when String.Caseless.equal word "price" ->
    Expr.Num.price state.ctx ~slug:(parse_known_slug state ~keyword:"price")
  | Word word when String.Caseless.equal word "inventory" ->
    Expr.Num.inventory
      state.ctx
      ~slug:(parse_known_slug state ~keyword:"inventory")
  | Word word when Option.is_some (find_slug state word) ->
    Expr.Num.price state.ctx ~slug:(slug_or_raw state word)
  | Lparen ->
    let inner = parse_num_expr state in
    expect state Rparen ~expecting:"')'";
    inner
  | token ->
    fail
      [%string "expected a numeric value, found '%{Token.to_string token}'"]
;;

(* Precedence, loosest to tightest: || then ^ then && then !. *)
let rec parse_bool_expr state =
  let lhs = parse_bool_xor state in
  let rec go lhs =
    match peek state with
    | Some Or_op ->
      advance state;
      go (Expr.Bool.or_ state.ctx lhs (parse_bool_xor state))
    | Some
        ( Number _ | Percent_lit _ | Duration _ | Word _ | Var _ | Lparen
        | Rparen | Assign | Eq | Ne | Ge | Le | Gt | Lt | And_op | Xor_op
        | Not_op | Plus | Minus | Star | Slash )
    | None ->
      lhs
  in
  go lhs

and parse_bool_xor state =
  let lhs = parse_bool_and state in
  let rec go lhs =
    match peek state with
    | Some Xor_op ->
      advance state;
      go (Expr.Bool.xor state.ctx lhs (parse_bool_and state))
    | Some
        ( Number _ | Percent_lit _ | Duration _ | Word _ | Var _ | Lparen
        | Rparen | Assign | Eq | Ne | Ge | Le | Gt | Lt | And_op | Or_op
        | Not_op | Plus | Minus | Star | Slash )
    | None ->
      lhs
  in
  go lhs

and parse_bool_and state =
  let lhs = parse_bool_not state in
  let rec go lhs =
    match peek state with
    | Some And_op ->
      advance state;
      go (Expr.Bool.and_ state.ctx lhs (parse_bool_not state))
    | Some
        ( Number _ | Percent_lit _ | Duration _ | Word _ | Var _ | Lparen
        | Rparen | Assign | Eq | Ne | Ge | Le | Gt | Lt | Or_op | Xor_op
        | Not_op | Plus | Minus | Star | Slash )
    | None ->
      lhs
  in
  go lhs

and parse_bool_not state =
  match peek state with
  | Some Not_op ->
    advance state;
    Expr.Bool.not_ state.ctx (parse_bool_not state)
  | Some
      ( Number _ | Percent_lit _ | Duration _ | Word _ | Var _ | Lparen
      | Rparen | Assign | Eq | Ne | Ge | Le | Gt | Lt | And_op | Or_op
      | Xor_op | Plus | Minus | Star | Slash )
  | None ->
    parse_bool_atom state

and parse_bool_atom state =
  (* A comparison of two numeric expressions is tried first: it also covers
     numeric parentheses and numeric [$vars], and backtracks cleanly if no
     comparison operator follows. *)
  match try_parse state ~f:parse_comparison with
  | Some comparison -> comparison
  | None ->
    (match next state ~expecting:"a boolean expression" with
     | Word word when String.Caseless.equal word "true" ->
       Expr.Bool.const state.ctx true
     | Word word when String.Caseless.equal word "false" ->
       Expr.Bool.const state.ctx false
     | Word word
       when String.Caseless.equal word "price"
            || String.Caseless.equal word "inventory" ->
       (* The comparison attempt above already failed, so either the
          reference itself is broken (unknown market — surface that error) or
          no comparison operator followed it. *)
       let keyword = String.lowercase word in
       let (_ : Slug.t) = parse_known_slug state ~keyword in
       fail
         [%string
           "%{keyword} is a numeric value; compare it to something to form \
            a condition"]
     | Var name -> or_fail (Var_env.find_bool state.vars name)
     | Lparen ->
       let inner = parse_bool_expr state in
       expect state Rparen ~expecting:"')'";
       inner
     | Word slug_word ->
       parse_signal state ~slug:(slug_or_raw state slug_word)
     | token ->
       fail
         [%string
           "expected a boolean expression, found '%{Token.to_string token}'"])

and parse_comparison state =
  let lhs = parse_num_expr state in
  let op : Expr.Cmp.t =
    match next state ~expecting:"a comparison operator" with
    | Gt -> Gt
    | Lt -> Lt
    | Ge -> Ge
    | Le -> Le
    | Eq -> Eq
    | Ne -> Ne
    | token ->
      fail
        [%string
          "expected a comparison operator, found '%{Token.to_string token}'"]
  in
  let rhs = parse_num_expr state in
  Expr.Bool.cmp state.ctx op lhs rhs

and parse_signal state ~slug =
  let contract = parse_contract state in
  let direction : Market_signal.Direction.t =
    match next state ~expecting:"up or down" with
    | Word word when String.Caseless.equal word "up" -> Up
    | Word word when String.Caseless.equal word "down" -> Down
    | token ->
      fail [%string "expected up or down, found '%{Token.to_string token}'"]
  in
  expect_word state "by";
  let by : Market_signal.Magnitude.t =
    match next state ~expecting:"an amount or percentage" with
    | Number value -> Amount value
    | Percent_lit value -> Percent value
    | token ->
      fail
        [%string
          "expected an amount like 0.05 or a percentage like 5%, found \
           '%{Token.to_string token}'"]
  in
  expect_word state "since";
  let start_ago = parse_duration state ~expecting:"a lookback duration" in
  if at_word state "ago" then advance state;
  let end_ago =
    match at_word state "end" with
    | false -> Time_ns.Span.zero
    | true ->
      advance state;
      let span = parse_duration state ~expecting:"a window-end duration" in
      if at_word state "ago" then advance state;
      span
  in
  Expr.Bool.signal
    state.ctx
    (Moved { slug; contract; direction; window = { start_ago; end_ago }; by })
;;

let parse_action state : Rule.Action_spec.t =
  let side = parse_side state in
  let size = parse_num_expr state in
  let slug = parse_slug state in
  let contract = parse_contract state in
  { side; size; slug; contract }
;;

let at_end state = state.pos >= Array.length state.tokens

let expect_end state =
  match peek state with
  | None -> ()
  | Some token ->
    fail
      [%string
        "unexpected '%{Token.to_string token}' after a complete statement"]
;;

let parse_variable_definition state ~name =
  (match find_slug state name with
   | Some (_ : Slug.t) ->
     fail [%string "variable name '%{name}' collides with a market ticker"]
   | None -> ());
  let binding : Var_env.Binding.t =
    let numeric =
      try_parse state ~f:(fun state ->
        let expr = parse_num_expr state in
        match at_end state with
        | true -> expr
        | false -> fail "numeric expression did not reach the end of line")
    in
    match numeric with
    | Some expr -> Num expr
    | None ->
      let expr = parse_bool_expr state in
      expect_end state;
      Bool expr
  in
  state.vars <- or_fail (Var_env.add state.vars ~name binding)
;;

let parse_rule state =
  let every = ref None in
  let when_i = ref None in
  let rec parse_qualifiers () =
    match at_word state "every", at_word state "when" with
    | true, _ ->
      advance state;
      (match !every with
       | Some (_ : Time_ns.Span.t) -> fail "duplicate every qualifier"
       | None ->
         every := Some (parse_duration state ~expecting:"an every interval");
         parse_qualifiers ())
    | false, true ->
      advance state;
      expect_word state "i";
      (match !when_i with
       | Some (_ : Rule.Trigger.t) -> fail "duplicate when i qualifier"
       | None ->
         let side = parse_side state in
         let slug = parse_slug state in
         when_i := Some { Rule.Trigger.side; slug };
         parse_qualifiers ())
    | false, false -> ()
  in
  parse_qualifiers ();
  let body : Rule.Body.t =
    match at_word state "if" with
    | false -> Always (parse_action state)
    | true ->
      advance state;
      let condition = parse_bool_expr state in
      expect_word state "then";
      let then_ = parse_action state in
      let else_ =
        match at_word state "else" with
        | false -> None
        | true ->
          advance state;
          Some (parse_action state)
      in
      Conditional { condition; then_; else_ }
  in
  expect_end state;
  or_fail (Rule.create ~every:!every ~when_i:!when_i body)
;;

(* A variable definition returns no rule; anything else must be a rule. *)
let parse_statement state : Rule.t option =
  let starts_definition =
    match Array.length state.tokens >= 2 with
    | false -> false
    | true ->
      (match state.tokens.(0), state.tokens.(1) with
       | Word (_ : string), Assign -> true
       | ( ( Number _ | Percent_lit _ | Duration _ | Word _ | Var _ | Lparen
           | Rparen | Assign | Eq | Ne | Ge | Le | Gt | Lt | And_op | Or_op
           | Xor_op | Not_op | Plus | Minus | Star | Slash )
         , (_ : Token.t) ) ->
         false)
  in
  match starts_definition with
  | true ->
    let name =
      match state.tokens.(0) with
      | Word name -> name
      | token ->
        fail [%string "impossible: %{Token.to_string token} not a name"]
    in
    state.pos <- 2;
    parse_variable_definition state ~name;
    None
  | false -> Some (parse_rule state)
;;

let program text ~slugs =
  let ctx = Expr.Context.create () in
  let%bind.Or_error slug_map = Ticker_name.build_map slugs in
  let vars = ref (Var_env.create ctx) in
  String.split_lines text
  |> List.filter_mapi ~f:(fun index line ->
    let line = String.strip line in
    match String.is_empty line with
    | true -> None
    | false ->
      let parsed =
        match Token.tokenize line with
        | Error error ->
          Or_error.error_s
            [%message
              "parse error"
                ~line:(index + 1 : int)
                (line : string)
                ~_:(error : Error.t)]
        | Ok tokens ->
          let state =
            { tokens = Array.of_list tokens
            ; pos = 0
            ; ctx
            ; vars = !vars
            ; slug_map
            }
          in
          (match parse_statement state with
           | exception Parse_error message ->
             Or_error.error_s
               [%message
                 "parse error"
                   ~line:(index + 1 : int)
                   (line : string)
                   ~_:(message : Sexp.t)]
           | rule ->
             vars := state.vars;
             Ok rule)
      in
      Some parsed)
  |> Or_error.combine_errors
  |> Or_error.map ~f:List.filter_opt
;;
