open! Core

module Range = struct
  type t =
    { lo : float
    ; hi : float
    }

  let padding_fraction = 0.05
  let degenerate_halfwidth = 0.5

  let of_values values =
    match
      ( List.min_elt values ~compare:Float.compare
      , List.max_elt values ~compare:Float.compare )
    with
    | None, _ | _, None ->
      { lo = -.degenerate_halfwidth; hi = degenerate_halfwidth }
    | Some lo, Some hi ->
      (match Float.O.(hi - lo > 0.) with
       | false ->
         { lo = lo -. degenerate_halfwidth; hi = hi +. degenerate_halfwidth }
       | true ->
         let padding = (hi -. lo) *. padding_fraction in
         { lo = lo -. padding; hi = hi +. padding })
  ;;
end

let fraction (range : Range.t) value =
  (value -. range.lo) /. (range.hi -. range.lo)
;;

let scale_x range ~extent value = fraction range value *. extent
let scale_y range ~extent value = (1. -. fraction range value) *. extent

let polyline points ~x_range ~y_range ~width ~height =
  List.map points ~f:(fun (x, y) ->
    let x = scale_x x_range ~extent:width x in
    let y = scale_y y_range ~extent:height y in
    sprintf "%.1f,%.1f" x y)
  |> String.concat ~sep:" "
;;

let label value = sprintf "%.2f" value
