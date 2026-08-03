open! Core
open Types

let volume_rank (stub : Market_stub.t) =
  match stub.volume with
  | None -> Float.neg_infinity
  | Some volume -> Volume.to_float volume
;;

let live_purge_victims stubs ~count =
  List.sort stubs ~compare:(Comparable.lift Float.compare ~f:volume_rank)
  |> (fun sorted -> List.take sorted count)
  |> List.map ~f:(fun (stub : Market_stub.t) -> stub.market_id)
;;

let select_new ~fetched ~existing_ids ~purged_ids ~needed =
  let candidates =
    List.filter fetched ~f:(fun (stub : Market_stub.t) ->
      not (Set.mem existing_ids stub.market_id))
  in
  let purged, fresh =
    List.partition_tf candidates ~f:(fun (stub : Market_stub.t) ->
      Set.mem purged_ids stub.market_id)
  in
  let fresh = List.take fresh needed in
  let readds = List.take purged (needed - List.length fresh) in
  fresh @ readds, List.length readds
;;
