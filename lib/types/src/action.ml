open! Core

type t =
  { size : Size.t
  ; side : Side.t
  ; slug : Slug.t
  ; contract_type : Contract_type.t
  ; id : int
  }

module Generator = struct
  type t = int ref
  type gen = t

  let create () = ref 0

  let new_action (t : t) ~size ~side ~slug ~contract_type =
    t := !t + 1;
    { size; side; slug; contract_type; id = !t }
  ;;
end
