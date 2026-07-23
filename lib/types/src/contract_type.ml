open! Core

type t =
  | Yes
  | No

let flip = function Yes -> No | No -> Yes
let sign = function Yes -> Sign.Pos | No -> Sign.Neg
