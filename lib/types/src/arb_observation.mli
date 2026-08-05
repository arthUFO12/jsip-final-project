open! Core

(** One sighting of a tradable arbitrage edge, appended every time a scan
    prices one — the raw material for a real (non-simulated) PnL-over-time
    line. Unlike the wallet's one-row-per-pair upsert (which overwrites
    [edge]/[dollars] on every later scan), this table only ever grows, so
    history survives. Persistence lives in the database library.

    [dollars] is [edge x size] at observation time: what taking the whole
    visible depth would have locked in. *)

type t =
  { at : Time_ns.t
  ; pair_key : string
  ; edge : float (** dollars per contract, after fees *)
  ; size : int (** contracts of depth visible at the time *)
  ; dollars : float (** [edge x size] *)
  }
[@@deriving sexp_of]
