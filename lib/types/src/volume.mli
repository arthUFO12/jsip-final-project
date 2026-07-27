(** Traded volume as reported by a venue.

    Venues disagree on the unit: Kalshi reports a contract count (e.g.
    [volume_fp = "114509.59"]), while Polymarket reports USD notional (e.g.
    [volumeNum = 877136.60]). The constructor records which one you have so
    the two are never compared or summed as if interchangeable. *)

open! Core

type t =
  | Contracts of Size.t (** Kalshi: contracts traded, rounded to whole *)
  | Notional of Price.t (** Polymarket: USD traded *)
[@@deriving sexp_of, compare, equal]
