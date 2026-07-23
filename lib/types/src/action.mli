open! Core

type t

(* Note: The ownership of this function will likely change *)
val perform_action : t -> unit

module Generator : sig
  type gen

  val create : unit -> gen

  val new_action
    :  gen
    -> name:string
    -> size:int
    -> contract_name:string
    -> contract_type:Contract_type.t
    -> t

  type t = gen
end
