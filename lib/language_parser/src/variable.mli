open! Core
open Types



type argument_type =
| Price of Price.t
| Size of Size.t
| Volume of Volume.t


type t =
  | Multiply of t * t
  | Divide of t * t
  | Negative of t
  | Add of t * t
  | Subtract of t * t
  | Constant of float
  | Argument of string