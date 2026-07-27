open! Core

module T = struct
  type t = int [@@deriving sexp, bin_io, compare, equal, hash]
  (* Invariant: value is in microcents. *)
end

include T
include Comparable.Make (T)

let microcents_per_cent = 1_000_000
let microcents_per_dollar = 100_000_000
let of_microcents n = n
let of_int_cents n = Int.( * ) n microcents_per_cent

let of_dollars_string str =
  Option.try_with (fun () ->
    let dollars_part, frac_part =
      match String.split str ~on:'.' with
      | [ l; r ] -> l, r
      | [ l ] -> l, ""
      | _ -> failwith "malformed price string"
    in
    if String.length frac_part > 8 then failwith "fraction too precise";
    if Char.equal dollars_part.[0] '-' then failwith "negative price";
    let dollars = Int.of_string dollars_part in
    let frac =
      if String.is_empty frac_part
      then 0
      else
        Int.( * )
          (Int.of_string frac_part)
          (Int.pow 10 (Int.( - ) 8 (String.length frac_part)))
    in
    Int.( + ) (Int.( * ) dollars microcents_per_dollar) frac)
;;

let to_microcents t = t

let of_float_dollars n =
  Float.iround_nearest_exn
    (Float.( * ) n (Float.of_int microcents_per_dollar))
;;

(* converting polymarket bids for parser *)

let to_int_cents_exn t =
  if Int.( <> ) (Int.rem t microcents_per_cent) 0
  then
    raise_s
      [%message
        "Price.to_int_cents_exn: not a whole number of cents" (t : int)];
  Int.( / ) t microcents_per_cent
;;

let to_dollar_float t =
  Float.( / ) (Float.of_int t) (Float.of_int microcents_per_dollar)
;;

let zero = 0
let ( + ) = Int.( + )
let ( - ) = Int.( - )
let ( * ) price qty = Int.( * ) price qty

let to_string_dollar t =
  let is_negative = Int.( < ) t 0 in
  let t_abs = Int.abs t in
  let dollars = Int.( / ) t_abs microcents_per_dollar in
  let frac = Int.rem t_abs microcents_per_dollar in
  let frac_str =
    let full = sprintf "%08d" frac in
    let trimmed = String.rstrip full ~drop:(Char.( = ) '0') in
    if Int.( < ) (String.length trimmed) 2
    then String.prefix full 2
    else trimmed
  in
  sprintf "%s$%d.%s" (if is_negative then "-" else "") dollars frac_str
;;

let neg = Int.neg
