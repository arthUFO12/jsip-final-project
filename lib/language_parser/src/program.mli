(** A validated set of {!Rule}s, compiled against a specific simulation: its
    tick span and its universe of market slugs. This is the [Or_error]
    boundary between rule construction and the bot — a [t] that exists has
    passed every static check, so the bot's tick loop never revalidates.

    Checks performed by {!create}:
    - every [EVERY] span is a positive integral multiple of [tick] (e.g.
      [EVERY 90m] on an hourly simulation is rejected, not rounded);
    - every signal-window offset is a non-negative integral multiple of
      [tick], with [start_ago >= end_ago];
    - every slug referenced anywhere (actions, WHEN-I triggers, price
      built-ins, signals) is one of [slugs]. *)

open! Core
open Types

module Compiled_rule : sig
  type t = private
    { rule : Rule.t
    ; every_ticks : int option
    (** [Some n]: only evaluate when [tick_index mod n = 0], where
        [tick_index] counts {e simulated} ticks from 0 (warmup ticks are
        never simulated, so the phase anchors at the first [on_tick]). *)
    }
end

type t

val create
  :  rules:Rule.t list
  -> tick:Time_ns.Span.t
  -> slugs:Slug.t list
  -> t Or_error.t

val rules : t -> Compiled_rule.t list
