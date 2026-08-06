open! Core
open Types

(* Missing volume ranks below any reported volume, however small. *)
let volume_key (card : Protocol.Market_card.t) =
  match card.volume with
  | None -> Float.neg_infinity
  | Some volume -> Protocol.Volume.to_float volume
;;

let descending_volume left right =
  Float.compare (volume_key right) (volume_key left)
;;

let keep_showable groups ~min_per_category =
  List.filter groups ~f:(fun (_, cards) ->
    List.length cards >= min_per_category)
;;

let group cards ~max_per_category ~min_per_category =
  cards
  |> List.sort_and_group
       ~compare:(fun (left : Protocol.Market_card.t) right ->
         Category.compare left.category right.category)
  |> List.map ~f:(fun cards ->
    let category = (List.hd_exn cards).Protocol.Market_card.category in
    let ranked = List.sort cards ~compare:descending_volume in
    category, ranked)
  |> keep_showable ~min_per_category
  |> List.map ~f:(fun (category, ranked) ->
    category, List.take ranked max_per_category)
  |> List.sort ~compare:(fun (_, left) (_, right) ->
    match left, right with
    | [], [] -> 0
    | [], _ :: _ -> 1
    | _ :: _, [] -> -1
    | left_top :: _, right_top :: _ -> descending_volume left_top right_top)
;;
