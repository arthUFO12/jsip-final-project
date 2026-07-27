open! Core

type t [@@deriving compare, equal]

module Generator : sig
  type gen

  val create : unit -> gen
  val generate : gen -> t

  type t = gen
end
