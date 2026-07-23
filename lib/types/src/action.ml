open! Core

type t =
  { name : string
  ; size : int
  ; contract_name : string
  ; contract_type : Contract_type.t
  ; id : int
  }

module Generator = struct
  type t = int ref
  type gen = t

  let create () = ref 0

  let new_action (t : t) ~name ~size ~contract_name ~contract_type =
    t := !t + 1;
    { name; size; contract_name; contract_type; id = !t }
  ;;
end

let perform_action (t : t) : unit = ignore t
