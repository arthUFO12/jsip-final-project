(** Fixed-point price representation.

    Prices are represented as an integer number of microcents (1 cent =
    1_000_000 microcents; 1 dollar = 100_000_000 microcents). For example,
    $67.67 is stored as 6_767_000_000.

    Microcents (10^-8 dollars) strictly exceed USDC's 6-decimal on-chain
    precision, so any value Polymarket can emit maps in losslessly. Venue
    string formats are not this module's concern; see [Exchange_parser]. *)

open! Core

type t [@@deriving sexp, bin_io, compare, equal, hash]

include Comparable.S with type t := t

(** {1 Scale constants} *)

val microcents_per_cent : int
val microcents_per_dollar : int

(** {4 Construction} *)

val of_microcents : int -> t

(** [of_int_cents 6767] represents $67.67 (stored as 6_767_000_000
    microcents). *)
val of_int_cents : int -> t

val of_float_dollars : float -> t
val of_dollars_string : string -> t option

(** {1 Accessors} *)

val to_microcents : t -> int

(** Raises if the price is not a whole number of cents. Sub-cent values must
    stay in microcents; this never rounds. *)
val to_int_cents_exn : t -> int

(** Lossy; for display and plotting only. Never use in PnL math. *)
val to_dollar_float : t -> float

(** {1 Arithmetic} *)

val zero : t
val neg : t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t

(** Scales a price by an integer quantity. Note: the result is a monetary
    amount rather than a quoted price; this may migrate to a [Cash] type. *)
val ( * ) : t -> int -> t

(** {1 Display} *)

(** e.g. "$67.67", "$0.505", "-$1.50". Trailing zeros trimmed to a minimum of
    two decimal places. *)
val to_string_dollar : t -> string
