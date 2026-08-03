(** The rule editor's {!Autocomplete} environment and its click-to-insert
    editing helpers.

    The environment holds the bot language's lowercase keywords
    ({!Parser.Ticker_name.keywords}), the builtin [$variables], the chosen
    market tickers in their {!Parser.Ticker_name.normalize}d program
    spelling, and — like the parser's [Var_env] — a [$name] for every
    [name = ...] definition already in the buffer, so a variable becomes
    completable the moment it is defined. Rebuild it on every keystroke; it
    is cheap. *)

open! Core
open Types

val env : tickers:Slug.t list -> program:string -> Autocomplete.t

(** The word in progress at the end of the buffer (letters, digits, [_], [-],
    [$]); empty when the buffer ends in whitespace or punctuation. *)
val current_token : string -> string

(** Replaces the {!current_token} at the end of [text] with [with_] plus a
    trailing space — what clicking a suggestion does. *)
val complete : string -> with_:string -> string
