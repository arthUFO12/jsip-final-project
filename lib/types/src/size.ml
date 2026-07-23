open! Core
include Int

let of_int = Fn.id
let to_int = Fn.id

let multiply_by_price size price =
  size * Price.to_microcents price |> Price.of_microcents
;;

let divide_price price size =
  Price.to_microcents price / to_int size |> Price.of_microcents
;;
