open! Core 


type t = int [@@deriving compare, equal]


module Generator = struct 
  type gen = int ref

  let create () : gen = ref 0

  let generate (gen : gen) : t= 
    gen := !gen + 1;
    !gen

  type t = gen
end