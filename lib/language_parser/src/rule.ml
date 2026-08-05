open! Core
open Types

module Action_spec = struct
  type t =
    { side : Side.t
    ; size : Expr.Num.t
    ; slug : Slug.t
    ; contract : Contract_type.t
    }
end

module Trigger = struct
  module T = struct
    type t =
      { side : Side.t
      ; slug : Slug.t
      }
    [@@deriving sexp_of, compare, equal, hash]
  end

  include T
  include Hashable.Make_plain (T)
end

module Body = struct
  type t =
    | Conditional of
        { condition : Expr.Bool.t
        ; then_ : Action_spec.t
        ; else_ : Action_spec.t option
        }
    | Always of Action_spec.t
end

type t =
  { every : Time_ns.Span.t option
  ; when_i : Trigger.t option
  ; body : Body.t
  }

let create ~every ~when_i body =
  match body, every, when_i with
  | Body.Always _, None, None ->
    Or_error.error_s
      [%message
        "a bare action statement requires an EVERY or WHEN I qualifier"]
  | (Always _ | Conditional _), _, _ ->
    (match every with
     | Some span when Time_ns.Span.( <= ) span Time_ns.Span.zero ->
       Or_error.error_s
         [%message "EVERY interval must be positive" (span : Time_ns.Span.t)]
     | Some _ | None -> Ok { every; when_i; body })
;;

let eval_justified t eval_ =
  match t.body with
  | Always spec -> Some (spec, [])
  | Conditional { condition; then_; else_ } ->
    (match Expr.Eval.bool eval_ condition with
     | true -> Some (then_, Expr.justify condition eval_ ~want:true)
     | false ->
       Option.map else_ ~f:(fun spec ->
         spec, Expr.justify condition eval_ ~want:false))
;;

let eval t eval_ = Option.map (eval_justified t eval_) ~f:fst

let action_specs t =
  match t.body with
  | Always spec -> [ spec ]
  | Conditional { condition = _; then_; else_ } ->
    then_ :: Option.to_list else_
;;

let referenced_slugs t =
  let of_spec ({ side = _; size; slug; contract = _ } : Action_spec.t) =
    slug :: Expr.Num.referenced_slugs size
  in
  let of_body =
    match t.body with
    | Always _ -> []
    | Conditional { condition; _ } -> Expr.Bool.referenced_slugs condition
  in
  let of_trigger =
    match t.when_i with None -> [] | Some { side = _; slug } -> [ slug ]
  in
  List.concat (of_body :: of_trigger :: List.map (action_specs t) ~f:of_spec)
  |> List.dedup_and_sort ~compare:Slug.compare
;;

let signals t =
  match t.body with
  | Always _ -> []
  | Conditional { condition; _ } -> Expr.Bool.signals condition
;;
