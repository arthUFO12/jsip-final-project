open! Core
open Types

(* Optional second opinion on candidates the string matcher found ambiguous.
   Sends a pair to a model, gets back yes/no plus a short explanation of
   which attributes drove the decision. Caches every verdict so the same pair
   is never asked twice and reruns are deterministic. Can be switched off
   entirely without affecting anything else. Its output is advisory it feeds
   the review step, never a trade. *)
