open! Core

type t =
  | Yes
  | No

let flip = function Yes -> No | No -> Yes
