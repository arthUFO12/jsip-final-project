open! Core
open Types

(* Answers "is there edge on this pair right now?" using prices only.

   Given a confirmed pair and the current book on each venue, works out both
   leg combinations, prices them at asks including fees, and reports
   whichever is profitable along with how much size is actually available.
   Returns nothing if there's no edge. Sees no titles. No I/O, no clock, no
   randomness — same inputs always give the same answer. *)
open Types

type direction =
  | Yes_kalshi_no_poly
  | Yes_poly_no_kalshi

type opportunity =
  { pair : Pair.t
  ; direction : direction
  ; cost : Price.t (* combined entry, asks + fees *)
  ; edge : Price.t (* 100c - cost *)
  ; size : Contracts.t (* min depth across both legs *)
  }

val find : Pair.t -> kalshi:Book.t -> poly:Book.t -> opportunity option
