(* Tests for {!Parser.Var_env}: built-in seeding, ordered user definitions,
   and the error cases the spec requires. *)

open! Core
open Types
open Parser

let slug = Slug.of_string "save-act"

let%expect_test "builtins are seeded, unknown names error" =
  let ctx = Expr.Context.create () in
  let env = Var_env.create ctx ~slugs:[ slug ] in
  List.iter
    [ "cash"; "realized"; "unrealized"; "save-act_price"; "moon" ]
    ~f:(fun name ->
      let found =
        match Var_env.find_num env name with
        | Ok _ -> "found"
        | Error error -> Error.to_string_hum error
      in
      print_endline [%string "%{name}: %{found}"]);
  [%expect
    {|
    cash: found
    realized: found
    unrealized: found
    save-act_price: found
    moon: ("unknown variable" (name moon))
    |}]
;;

let%expect_test "definitions resolve in order and share nodes" =
  let ctx = Expr.Context.create () in
  let env = Var_env.create ctx ~slugs:[ slug ] in
  let open Or_error.Let_syntax in
  let result =
    let%bind cash = Var_env.find_num env "cash" in
    let double_cash = Expr.Num.mul ctx cash (Expr.Num.const ctx 2.) in
    let%bind env = Var_env.add env ~name:"double_cash" (Num double_cash) in
    (* A later definition can use the earlier one. *)
    let%bind double_cash_again = Var_env.find_num env "double_cash" in
    let quad = Expr.Num.mul ctx double_cash_again (Expr.Num.const ctx 2.) in
    let%bind env = Var_env.add env ~name:"quad_cash" (Num quad) in
    let%bind first = Var_env.find_num env "double_cash" in
    let%bind second = Var_env.find_num env "double_cash" in
    return (Expr.Num.id first = Expr.Num.id second)
  in
  print_s [%sexp (result : bool Or_error.t)];
  [%expect {| (Ok true) |}]
;;

let%expect_test "duplicate names and kind mismatches error" =
  let ctx = Expr.Context.create () in
  let env = Var_env.create ctx ~slugs:[] in
  let flag = Expr.Bool.const ctx true in
  let env = Var_env.add env ~name:"flag" (Bool flag) |> Or_error.ok_exn in
  print_s
    [%sexp
      (Var_env.add env ~name:"cash" (Bool flag) |> Or_error.map ~f:ignore
       : unit Or_error.t)];
  [%expect {| (Error ("variable is already defined" (name cash))) |}];
  print_s [%sexp (Var_env.find_num env "flag" : Expr.Num.t Or_error.t)];
  [%expect
    {|
    (Error
     ("variable used where a numeric value is required" (name flag)
      (defined_as boolean)))
    |}];
  print_s [%sexp (Var_env.find_bool env "cash" : Expr.Bool.t Or_error.t)];
  [%expect
    {|
    (Error
     ("variable used where a boolean value is required" (name cash)
      (defined_as numeric)))
    |}]
;;
