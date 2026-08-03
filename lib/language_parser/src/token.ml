open! Core

type t =
  | Number of float
  | Percent_lit of float
  | Duration of Time_ns.Span.t
  | Word of string
  | Var of string
  | Lparen
  | Rparen
  | Assign
  | Eq
  | Ne
  | Ge
  | Le
  | Gt
  | Lt
  | And_op
  | Or_op
  | Xor_op
  | Not_op
  | Plus
  | Minus
  | Star
  | Slash
[@@deriving sexp_of, equal]

let to_string = function
  | Number value -> Float.to_string value
  | Percent_lit value -> [%string "%{value#Float}%"]
  | Duration span -> Time_ns.Span.to_string span
  | Word word -> word
  | Var name -> [%string "$%{name}"]
  | Lparen -> "("
  | Rparen -> ")"
  | Assign -> "="
  | Eq -> "=="
  | Ne -> "!="
  | Ge -> ">="
  | Le -> "<="
  | Gt -> ">"
  | Lt -> "<"
  | And_op -> "&&"
  | Or_op -> "||"
  | Xor_op -> "^"
  | Not_op -> "!"
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
;;

(* Internal to the scanning loop; [tokenize] converts to [Or_error]. *)
exception Lex_error of string

let lex_fail message = raise (Lex_error message)
let is_word_start c = Char.is_alpha c || Char.equal c '_'
let is_word_char c = Char.is_alphanum c || Char.equal c '_'

(* Words take a '-' only when it glues two word characters together
   ([save-act]); otherwise '-' is subtraction. *)
let scan_word line start =
  let n = String.length line in
  let rec go i =
    match i < n with
    | false -> i
    | true ->
      (match
         is_word_char line.[i]
         || (Char.equal line.[i] '-'
             && i + 1 < n
             && is_word_char line.[i + 1])
       with
       | true -> go (i + 1)
       | false -> i)
  in
  let finish = go start in
  String.sub line ~pos:start ~len:(finish - start), finish
;;

let scan_number line start =
  let n = String.length line in
  let rec go i ~seen_dot =
    match i < n with
    | false -> i
    | true ->
      (match line.[i] with
       | '0' .. '9' -> go (i + 1) ~seen_dot
       | '.' when not seen_dot -> go (i + 1) ~seen_dot:true
       | _ -> i)
  in
  let finish = go start ~seen_dot:false in
  let value =
    Float.of_string (String.sub line ~pos:start ~len:(finish - start))
  in
  value, finish
;;

let duration_unit value = function
  | 'm' -> Time_ns.Span.of_min value
  | 'h' -> Time_ns.Span.of_hr value
  | 'd' -> Time_ns.Span.of_day value
  | unit -> lex_fail [%string "unknown duration unit '%{unit#Char}'"]
;;

let tokenize_exn line : t list =
  let n = String.length line in
  let rec go i acc =
    match i < n with
    | false -> List.rev acc
    | true ->
      let two = if i + 1 < n then Some line.[i + 1] else None in
      (match line.[i] with
       | ' ' | '\t' -> go (i + 1) acc
       | '(' -> go (i + 1) (Lparen :: acc)
       | ')' -> go (i + 1) (Rparen :: acc)
       | '+' -> go (i + 1) (Plus :: acc)
       | '-' -> go (i + 1) (Minus :: acc)
       | '*' -> go (i + 1) (Star :: acc)
       | '/' -> go (i + 1) (Slash :: acc)
       | '^' -> go (i + 1) (Xor_op :: acc)
       | '=' when Option.equal Char.equal two (Some '=') ->
         go (i + 2) (Eq :: acc)
       | '=' -> go (i + 1) (Assign :: acc)
       | '!' when Option.equal Char.equal two (Some '=') ->
         go (i + 2) (Ne :: acc)
       | '!' -> go (i + 1) (Not_op :: acc)
       | '>' when Option.equal Char.equal two (Some '=') ->
         go (i + 2) (Ge :: acc)
       | '>' -> go (i + 1) (Gt :: acc)
       | '<' when Option.equal Char.equal two (Some '=') ->
         go (i + 2) (Le :: acc)
       | '<' -> go (i + 1) (Lt :: acc)
       | '&' when Option.equal Char.equal two (Some '&') ->
         go (i + 2) (And_op :: acc)
       | '|' when Option.equal Char.equal two (Some '|') ->
         go (i + 2) (Or_op :: acc)
       | '$' ->
         (match i + 1 < n && is_word_start line.[i + 1] with
          | false -> lex_fail "expected a variable name after '$'"
          | true ->
            let word, next = scan_word line (i + 1) in
            go next (Var word :: acc))
       | '0' .. '9' ->
         let value, next = scan_number line i in
         let boundary j = j >= n || not (is_word_char line.[j]) in
         (match next < n, two with
          | true, _ when Char.equal line.[next] '%' ->
            go (next + 1) (Percent_lit value :: acc)
          | true, _
            when match line.[next] with
                 | 'm' | 'h' | 'd' -> boundary (next + 1)
                 | _ -> false ->
            go (next + 1) (Duration (duration_unit value line.[next]) :: acc)
          | _, _ -> go next (Number value :: acc))
       | c when is_word_start c ->
         let word, next = scan_word line i in
         go next (Word word :: acc)
       | c -> lex_fail [%string "unexpected character '%{c#Char}'"])
  in
  go 0 []
;;

let tokenize line =
  match tokenize_exn line with
  | tokens -> Ok tokens
  | exception Lex_error message -> Or_error.error_s [%message message]
;;
