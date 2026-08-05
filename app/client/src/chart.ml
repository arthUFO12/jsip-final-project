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

let unscale_x (range : Range.t) ~extent value =
  range.lo +. (value /. extent *. (range.hi -. range.lo))
;;

let nearest points ~x =
  match Array.length points with
  | 0 -> None
  | length ->
    (* First index whose x is >= [x]; [length] when none is. *)
    let lo = ref 0
    and hi = ref length in
    while !lo < !hi do
      let mid = (!lo + !hi) / 2 in
      match Float.O.(fst points.(mid) < x) with
      | true -> lo := mid + 1
      | false -> hi := mid
    done;
    let insertion = !lo in
    let candidates =
      List.filter [ insertion - 1; insertion ] ~f:(fun i ->
        i >= 0 && i < length)
    in
    List.min_elt candidates ~compare:(fun a b ->
      Float.compare
        (Float.abs (fst points.(a) -. x))
        (Float.abs (fst points.(b) -. x)))
    |> Option.map ~f:(fun i -> points.(i))
;;

let polyline points ~x_range ~y_range ~width ~height =
  List.map points ~f:(fun (x, y) ->
    let x = scale_x x_range ~extent:width x in
    let y = scale_y y_range ~extent:height y in
    sprintf "%.1f,%.1f" x y)
  |> String.concat ~sep:" "
;;

let label value = sprintf "%.2f" value
