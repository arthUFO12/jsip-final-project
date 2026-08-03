(** Lexical tokens of the bot-configuration language, and the tokenizer that
    produces them — one line of source at a time, since statements are
    line-delimited. {!Parse} consumes these; nothing here knows about
    grammar.

    Lexical commitments (shared with the grammar notes in {!Parse}):
    - Words take a '-' only when it glues two word characters together
      ([save-act]); otherwise '-' is the subtraction operator, so [$a - $b]
      subtracts while [$a-b] is one name.
    - A number immediately followed by [m], [h], or [d] at a word boundary
      lexes as a {!constructor:Duration} ([90m], [2h], [1d]); followed by [%]
      it lexes as a {!constructor:Percent_lit} ([5%]).
    - [==] is a comparison, a lone [=] the definition sign; likewise [!=] vs
      [!]. *)

open! Core

type t =
  | Number of float
  | Percent_lit of float
  | Duration of Time_ns.Span.t
  | Word of string (** Keyword, slug, or bare variable name. *)
  | Var of string (** [$name] *)
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

(** How the token spells in source, for error messages. *)
val to_string : t -> string

val tokenize : string -> t list Or_error.t
