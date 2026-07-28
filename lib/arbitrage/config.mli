open! Core
open Types

(* One record holding every knob the user can turn: trading mode, matching
   threshold, poll interval, stake per opportunity, fill model, whether the
   LLM stage is on. Parses it from a file or flags and validates it once at
   startup. Everything downstream receives config as data — nothing reads it
   from a global. *)
