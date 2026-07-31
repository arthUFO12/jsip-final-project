(** Traded volume as reported by a venue.

    Venues disagree on the unit: Kalshi reports a contract count (e.g.
    [volume_fp = "114509.59"]), while Polymarket reports USD notional (e.g.
    [volumeNum = 877136.60]). The constructor records which one you have so
    the two are never compared or summed as if interchangeable. *)

open! Core

type t =
  | Contracts of Size.t (** Kalshi: contracts traded, rounded to whole *)
  | Notional of Price.t (** Polymarket: USD traded *)
[@@deriving sexp, bin_io, compare, equal, hash]

(** The bare magnitude (contract count or dollars), for ranking markets by
    activity. Only meaningful as an ordering key within one venue — the units
    differ across constructors. *)
val to_float : t -> float
