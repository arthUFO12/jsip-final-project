(** Named bindings for the language's [$name] variables.

    Definitions resolve by substitution at construction time: a definition
    [NAME = expr] is built with only the earlier bindings visible, and the
    resulting {!Expr} {e node} is stored. Later [$NAME] references reuse that
    node, so acyclicity and define-before-use are enforced structurally, and
    — because nodes are hash-consed and evaluation is memoized — a variable
    used by many rules is computed at most once per tick. After construction
    no names remain: evaluation never looks anything up here.

    {!create} seeds the spec's built-ins: [cash], [realized], and
    [unrealized]. Market references (bare ticker, [PRICE], [INVENTORY],
    [AVGCOST]) are grammar forms handled by {!Parse}, not variables. *)

open! Core

module Binding : sig
  type t =
    | Num of Expr.Num.t
    | Bool of Expr.Bool.t
end

type t

val create : Expr.Context.t -> t

(** Errors on a duplicate name (including shadowing a built-in). *)
val add : t -> name:string -> Binding.t -> t Or_error.t

(** [find_num]/[find_bool] error on unknown names and on a binding of the
    other kind (e.g. [$flag * 2] where [flag] is boolean). *)

val find_num : t -> string -> Expr.Num.t Or_error.t
val find_bool : t -> string -> Expr.Bool.t Or_error.t
val find : t -> string -> Binding.t Or_error.t
