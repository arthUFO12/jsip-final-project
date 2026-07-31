open! Core
open Types

module Binding = struct
  type t =
    | Num of Expr.Num.t
    | Bool of Expr.Bool.t

  let kind = function Num _ -> "numeric" | Bool _ -> "boolean"
end

type t = Binding.t String.Map.t

let price_builtin_name slug = [%string "%{slug#Slug}_price"]

let create ctx ~slugs =
  let builtins =
    [ "cash", Binding.Num (Expr.Num.cash ctx)
    ; "realized", Num (Expr.Num.realized ctx)
    ; "unrealized", Num (Expr.Num.unrealized ctx)
    ]
    @ List.map slugs ~f:(fun slug ->
      price_builtin_name slug, Binding.Num (Expr.Num.price ctx ~slug))
  in
  String.Map.of_alist_exn builtins
;;

let add t ~name binding =
  match Map.add t ~key:name ~data:binding with
  | `Ok t -> Ok t
  | `Duplicate ->
    Or_error.error_s [%message "variable is already defined" (name : string)]
;;

let find t name =
  match Map.find t name with
  | Some binding -> Ok binding
  | None -> Or_error.error_s [%message "unknown variable" (name : string)]
;;

let find_num t name =
  match find t name with
  | Error _ as error -> error
  | Ok (Binding.Num num) -> Ok num
  | Ok (Bool _ as binding) ->
    Or_error.error_s
      [%message
        "variable used where a numeric value is required"
          (name : string)
          ~defined_as:(Binding.kind binding : string)]
;;

let find_bool t name =
  match find t name with
  | Error _ as error -> error
  | Ok (Binding.Bool bool_) -> Ok bool_
  | Ok (Num _ as binding) ->
    Or_error.error_s
      [%message
        "variable used where a boolean value is required"
          (name : string)
          ~defined_as:(Binding.kind binding : string)]
;;
