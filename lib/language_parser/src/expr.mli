(** Numeric and boolean expression DAGs for the bot-configuration language.

    Expressions are immutable and hash-consed: every node is built through a
    {!Context}, which assigns each structurally distinct node a unique int id
    and returns the {e same} node for structurally equal constructions.
    Subtrees are therefore shared — a variable used in five rules is one node
    — and node identity is the id, not physical equality.

    Nothing evaluates at construction time. Each tick the bot creates one
    {!Eval.t} (a fresh memo table over node ids) and evaluates rule roots
    top-down through it: shared nodes compute once per tick, short-circuited
    or unreachable nodes not at all, and no per-tick state ever lives on the
    nodes themselves.

    Numeric values are float dollars — see {!Eval_env} for why. *)

open! Core
open Types

module Context : sig
  (** Hash-consing state and id counter. Use one context per program; nodes
      from different contexts must not be mixed (ids would collide). *)
  type t

  val create : unit -> t
end

module Num : sig
  type t [@@deriving sexp_of]

  (** Unique within the node's {!Context.t}. *)
  val id : t -> int

  val const : Context.t -> float -> t

  (** The built-in variables: latest cash / realized pnl / unrealized pnl
      known to the bot, in dollars. *)

  val cash : Context.t -> t
  val realized : Context.t -> t
  val unrealized : Context.t -> t

  (** The [<slug>_price] built-in: the slug's current Yes price. *)
  val price : Context.t -> slug:Slug.t -> t

  val add : Context.t -> t -> t -> t
  val sub : Context.t -> t -> t -> t
  val mul : Context.t -> t -> t -> t
  val div : Context.t -> t -> t -> t
  val neg : Context.t -> t -> t

  (** Slugs read by [price] nodes anywhere in the DAG, deduplicated. *)
  val referenced_slugs : t -> Slug.t list
end

module Cmp : sig
  type t =
    | Gt
    | Lt
    | Ge
    | Le
    | Eq (** Exact float equality, as the spec's [==]. *)
    | Ne
  [@@deriving sexp_of, compare, equal, hash]
end

module Bool : sig
  type t [@@deriving sexp_of]

  (** Unique within the node's {!Context.t}; ids are shared with {!Num.id}
      (one counter per context). *)
  val id : t -> int

  val const : Context.t -> bool -> t
  val and_ : Context.t -> t -> t -> t
  val or_ : Context.t -> t -> t -> t
  val xor : Context.t -> t -> t -> t
  val not_ : Context.t -> t -> t
  val cmp : Context.t -> Cmp.t -> Num.t -> Num.t -> t
  val signal : Context.t -> Market_signal.t -> t

  (** Slugs read anywhere in the DAG (price built-ins and signals),
      deduplicated. *)
  val referenced_slugs : t -> Slug.t list

  (** Every distinct signal leaf in the DAG, for {!Program} validation. *)
  val signals : t -> Market_signal.t list
end

module Eval : sig
  (** One [t] per tick: fresh memo tables over node ids, discarded at tick
      end. Evaluation is demand-driven from the roots, so
      [and_ ctx cheap_false expensive] never touches [expensive]. *)
  type t

  val create : Eval_env.t -> t
  val num : t -> Num.t -> float
  val bool : t -> Bool.t -> bool
end
