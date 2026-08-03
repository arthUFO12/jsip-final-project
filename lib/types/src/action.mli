open! Core

type t =
  { size : Size.t (** Always non-negative; direction is carried by [side]. *)
  ; side : Side.t
  ; slug : Slug.t (** The market the action trades in. *)
  ; contract_type : Contract_type.t
  ; id : int
  (** Assigned by {!Generator.new_action}; unique per generator. *)
  }

module Generator : sig
  type gen

  val create : unit -> gen

  val new_action
    :  gen
    -> size:Size.t
    -> side:Side.t
    -> slug:Slug.t
    -> contract_type:Contract_type.t
    -> t

  type t = gen
end
