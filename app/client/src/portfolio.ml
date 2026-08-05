open! Core
open Types

let value ~inventory ~yes_prices =
  List.fold yes_prices ~init:0. ~f:(fun acc (slug, price) ->
    match List.Assoc.find inventory slug ~equal:Slug.equal with
    | None | Some 0 -> acc
    | Some held when held < 0 ->
      acc +. (Float.of_int (abs held) *. (1. -. price))
    | Some held -> acc +. (Float.of_int held *. price))
;;
