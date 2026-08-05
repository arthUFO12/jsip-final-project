(** One rule from a bot-configuration program:

    {v [EVERY n(m|h|d)] [WHEN I (BUY|SELL) slug] body v}

    where body is [IF cond THEN action [ELSE action]] or, when at least one
    qualifier is present, a bare action. Rules are pure descriptions; the bot
    decides each tick which rules' qualifiers pass (see
    {!Program.Compiled_rule} for EVERY bookkeeping and the configurable bot
    for WHEN-I trigger timing) and calls {!eval} on those. *)

open! Core
open Types

module Action_spec : sig
  (** What to trade when a rule fires. [size] is evaluated at firing time and
      rounded to whole contracts by the bot. *)
  type t =
    { side : Side.t
    ; size : Expr.Num.t
    ; slug : Slug.t
    ; contract : Contract_type.t
    }
end

module Trigger : sig
  (** The [WHEN I (BUY|SELL) slug] qualifier. Matches the bot's own
      {e accepted} actions; a rejected action triggers nothing. *)
  type t =
    { side : Side.t
    ; slug : Slug.t
    }
  [@@deriving sexp_of, compare, equal, hash]

  include Hashable.S_plain with type t := t
end

module Body : sig
  type t =
    | Conditional of
        { condition : Expr.Bool.t
        ; then_ : Action_spec.t
        ; else_ : Action_spec.t option
        }
    | Always of Action_spec.t
    (** A bare action statement; only legal behind a qualifier. *)
end

type t = private
  { every : Time_ns.Span.t option
  ; when_i : Trigger.t option
  ; body : Body.t
  }

(** Errors if [body] is [Always _] with no qualifier (the rule would fire
    unconditionally every tick, which the spec does not allow) or if [every]
    is not positive. *)
val create
  :  every:Time_ns.Span.t option
  -> when_i:Trigger.t option
  -> Body.t
  -> t Or_error.t

(** Assumes the qualifiers already passed this tick. [None] when the
    condition is false and there is no [ELSE]. *)
val eval : t -> Expr.Eval.t -> Action_spec.t option

(** {!eval}, plus why: the leaf conditions that decided the branch (see
    {!Expr.justify}). For a fired [ELSE] the atoms justify the condition's
    falsity; [Always] bodies have no condition, so their list is empty — the
    qualifier is the whole story. *)
val eval_justified
  :  t
  -> Expr.Eval.t
  -> (Action_spec.t * Expr.Atom.t list) option

(** Every slug the rule touches (condition, size, action target, trigger),
    for {!Program} validation. *)
val referenced_slugs : t -> Slug.t list

(** Every signal in the rule's condition, for {!Program} validation. *)
val signals : t -> Market_signal.t list
