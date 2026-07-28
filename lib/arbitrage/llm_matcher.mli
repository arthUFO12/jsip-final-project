open! Core
open Types

type verdict =
  { is_match : bool
  ; explanation : string
  }

val adjudicate : Candidate.t -> verdict Deferred.t (* cached *)
