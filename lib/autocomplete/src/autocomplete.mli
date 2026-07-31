(** A candidate environment for text autocompletion, in the spirit of the
    parser's {!Parser.Var_env}: seed it with a base vocabulary, grow it as
    new names appear, and query it with whatever the user has typed so far.

    The module knows nothing about where candidates come from — the web
    client seeds one env with market titles and tickers for the search bar,
    and another with the rule language's keywords, tickers, and user-defined
    [$variables] for the rule editor.

    {[
      let env = Autocomplete.of_list [ "IF"; "THEN"; "save-act" ] in
      Autocomplete.suggest env ~input:"sa" (* => ["save-act"] *)
    ]} *)

open! Core

type t

val empty : t

(** Candidates keep their first-occurrence order and spelling; duplicates
    (case-insensitive) are dropped. *)
val of_list : string list -> t

val add : t -> string -> t
val candidates : t -> string list

(** Candidates matching [input] case-insensitively, best first: prefix
    matches rank above substring matches, ties break alphabetically. An exact
    match is omitted (nothing to complete), as is everything when [input] is
    empty or whitespace. At most [limit] results. *)
val suggest : ?limit:int (* default 8 *) -> t -> input:string -> string list
