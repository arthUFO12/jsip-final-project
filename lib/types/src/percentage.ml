open! Core 



type t = float [@@deriving compare, equal]



let of_float (f : float) : t =
  if Float.O.(f > 1.) || Float.O.(f < 0.) then
    failwith "invalid percentage"
  else 
    f

let to_float (t : t) : float =
  t