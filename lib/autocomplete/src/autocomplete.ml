open! Core

(* First-occurrence order, first-occurrence spelling; deduped
   case-insensitively via the companion set. *)
type t =
  { ordered : string list (* newest first; [candidates] reverses *)
  ; seen : String.Caseless.Set.t
  }

let empty = { ordered = []; seen = String.Caseless.Set.empty }

let add t candidate =
  match Set.mem t.seen candidate with
  | true -> t
  | false ->
    { ordered = candidate :: t.ordered; seen = Set.add t.seen candidate }
;;

let of_list candidates = List.fold candidates ~init:empty ~f:add
let candidates t = List.rev t.ordered
let default_limit = 8

(* Orders two candidates that both matched [input] (already lowercased):
   which should the user see first? *)
let compare_matches ~input left right =
  let to_tuple word =
    let word = String.lowercase word in
    not (String.is_prefix word ~prefix:input), word
  in
  Tuple2.compare
    ~cmp1:Bool.compare
    ~cmp2:String.compare
    (to_tuple left)
    (to_tuple right)
;;

let suggest ?(limit = default_limit) t ~input =
  let input = String.strip input |> String.lowercase in
  match String.is_empty input with
  | true -> []
  | false ->
    candidates t
    |> List.filter ~f:(fun candidate ->
      let lower = String.lowercase candidate in
      String.is_substring lower ~substring:input
      && not (String.equal lower input))
    |> List.sort ~compare:(compare_matches ~input)
    |> fun ranked -> List.take ranked limit
;;
