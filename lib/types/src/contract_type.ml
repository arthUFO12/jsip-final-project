open! Core

type t =
  | Yes
  | No
[@@deriving sexp_of, compare, equal, hash]

let flip = function Yes -> No | No -> Yes
let sign = function Yes -> Sign.Pos | No -> Sign.Neg

let ( = ) t1 t2 =
  match t1, t2 with Yes, Yes | No, No -> true | Yes, No | No, Yes -> false
;;
