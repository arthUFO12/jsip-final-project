(** How market tickers are spelled inside bot programs.

    Venue slugs are unfriendly to type: Kalshi tickers are uppercase with
    hyphens (sometimes doubled, like [KXALBUMRELEASEDATETRIPPIE--26OCT01]),
    and a hyphen doubles as subtraction in the language. Programs therefore
    refer to markets by a normalized name — lowercase, every [-] replaced by
    [_] — and {!Parse.program} maps those names back to the original
    {!Types.Slug.t}, which stays untouched everywhere else (URLs, database
    keys, the wire).

    {v
    normalize "KXELONMARS-99"    = "kxelonmars_99"
    normalize "KXALBUM--26OCT01" = "kxalbum__26oct01"
    v} *)

open! Core
open Types

(** Canonical (lowercase) spelling of every language keyword. The parser
    accepts any casing; autocomplete and docs display these. *)
val keywords : string list

(** Lowercases and replaces each [-] with [_]. Applied both to slugs (to
    build the map) and to words read from programs (to look them up), so raw
    single-hyphen spellings like [save-act] still resolve. *)
val normalize : string -> string

(** Normalized name -> original slug, for every market in a simulation.
    Errors when two slugs normalize to the same name, when a normalized name
    spells a keyword, or when a name could not be typed as a single word (it
    must start with a letter or [_] and contain only letters, digits, and
    [_]). *)
val build_map : Slug.t list -> Slug.t String.Map.t Or_error.t
