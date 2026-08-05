(** A bot the user chose to keep: everything the bots page needs to rebuild
    the Pick and Rules stages — markets, rules, variables, and the simulation
    config (deliberately not the compare-against-dumb-bots settings, which
    stay per-run).

    The frontend stores a [t list] in the browser's localStorage as the sexp
    text from {!list_to_string}; {!list_of_string} reads it back. Config
    fields are kept as the raw input strings the bots page edits
    ([lookback = "14"]), and [interval] as {!Protocol.Interval.name}'s
    spelling ([Interval.t] has no [t_of_sexp]) — the page converts on load,
    exactly as it does for the interval dropdown. *)

open! Core
open Types

type t =
  { name : string (** what the saved-bots dropdown shows; unique per list *)
  ; slugs : Slug.t list
  ; rules : string list
  ; variables : string list
  ; interval : string (** a {!Protocol.Interval.name}, e.g. ["hour"] *)
  ; lookback : string (** days, as typed *)
  ; warmup : string (** hours, as typed *)
  ; starting_cash : string (** dollars, as typed *)
  ; allow_negative : bool
  }
[@@deriving sexp, equal]

(** Save [bot] into the list: a bot already saved under the same name is
    replaced, otherwise the new bot is added. *)
val upsert : t list -> t -> t list

val find : t list -> name:string -> t option

(** The list without any bot of that name; unchanged if none matches. *)
val remove : t list -> name:string -> t list

val list_to_string : t list -> string

(** Inverse of {!list_to_string}. Storage is user-reachable (browser dev
    tools), so unparseable text is treated as no saved bots rather than an
    error. *)
val list_of_string : string -> t list
