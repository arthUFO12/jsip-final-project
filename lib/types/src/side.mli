open! Core

type t =
  | Buy
  | Sell

val flip : t -> t
