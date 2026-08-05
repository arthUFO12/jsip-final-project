open! Core
open Types

module Episode = struct
  type t =
    { entered_s : float
    ; exited_s : float option
    ; entry_edge : float
    ; locked_dollars : float
    }
  [@@deriving sexp_of]
end

let dollars price = Price.to_dollar_float price
let fee venue price = dollars (Execution.Fees.taker_fee venue price)

(* Buying NO at the complement of the venue's YES mid; the fee applies to
   the NO price actually paid. *)
let split_cost ~yes_venue ~yes ~no_venue ~other_yes =
  let no_price = Price.of_float_dollars (1. -. dollars other_yes) in
  dollars yes +. fee yes_venue yes +. dollars no_price +. fee no_venue no_price
;;

let edge_after_fees ~venue_a ~yes_a ~venue_b ~yes_b =
  let yes_on_a =
    split_cost ~yes_venue:venue_a ~yes:yes_a ~no_venue:venue_b ~other_yes:yes_b
  in
  let yes_on_b =
    split_cost ~yes_venue:venue_b ~yes:yes_b ~no_venue:venue_a ~other_yes:yes_a
  in
  1. -. Float.min yes_on_a yes_on_b
;;

let episodes ~points ~min_edge ~stake =
  let stake = Float.of_int stake in
  let armed, open_episode, closed =
    List.fold
      points
      ~init:(true, None, [])
      ~f:(fun (armed, open_episode, closed) (time_s, edge) ->
        match armed, open_episode with
        | true, None when Float.O.(edge >= min_edge) ->
          ( false
          , Some
              { Episode.entered_s = time_s
              ; exited_s = None
              ; entry_edge = edge
              ; locked_dollars = edge *. stake
              }
          , closed )
        | false, Some episode when Float.O.(edge < min_edge) ->
          (true, None, { episode with exited_s = Some time_s } :: closed)
        | (true | false), _ -> armed, open_episode, closed)
  in
  ignore (armed : bool);
  List.rev (Option.to_list open_episode @ closed)
;;

let cumulative episodes =
  List.folding_map episodes ~f:(fun total (episode : Episode.t) ->
    let total = total +. episode.locked_dollars in
    total, (episode.entered_s, total))
    ~init:0.
;;

let observation_episodes ~sightings ~gap_s =
  let by_pair =
    List.sort_and_group sightings ~compare:(fun (_, pair_a, _) (_, pair_b, _) ->
      String.compare pair_a pair_b)
  in
  let entries =
    List.concat_map by_pair ~f:(fun sightings ->
      let sightings =
        List.sort sightings ~compare:(fun (time_a, _, _) (time_b, _, _) ->
          Float.compare time_a time_b)
      in
      List.folding_map
        sightings
        ~init:None
        ~f:(fun previous (time_s, (_ : string), dollars) ->
          let fresh =
            match previous with
            | None -> true
            | Some previous_s -> Float.O.(time_s -. previous_s > gap_s)
          in
          Some time_s, Option.some_if fresh (time_s, dollars))
      |> List.filter_opt)
  in
  List.sort entries ~compare:(fun (time_a, _) (time_b, _) ->
    Float.compare time_a time_b)
  |> List.folding_map ~init:0. ~f:(fun total (time_s, dollars) ->
    let total = total +. dollars in
    total, (time_s, total))
;;
