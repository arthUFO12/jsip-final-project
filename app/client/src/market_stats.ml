open! Core

let trailing_sum ~window_s points =
  List.map points ~f:(fun (time, _) ->
    let sum =
      List.sum (module Float) points ~f:(fun (time', value) ->
        match
          Float.O.(time' <= time) && Float.O.(time' > time -. window_s)
        with
        | true -> value
        | false -> 0.)
    in
    time, sum)
;;

let volatility values =
  let diffs =
    match values with
    | [] -> []
    | _ :: rest ->
      List.map2_exn (List.drop_last_exn values) rest ~f:(fun a b -> b -. a)
  in
  match List.length diffs < 2 with
  | true -> 0.
  | false ->
    let count = Float.of_int (List.length diffs) in
    let mean = List.sum (module Float) diffs ~f:Fn.id /. count in
    let variance =
      List.sum (module Float) diffs ~f:(fun diff ->
        let centered = diff -. mean in
        centered *. centered)
      /. count
    in
    Float.sqrt variance
;;
