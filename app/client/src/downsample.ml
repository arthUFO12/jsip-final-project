open! Core

let downsample points ~max_points =
  (match max_points < 4 with
   | true ->
     raise_s [%message "downsample budget too small" (max_points : int)]
   | false -> ());
  let n = List.length points in
  match n <= max_points with
  | true -> points
  | false ->
    let arr = Array.of_list points in
    (* Two slots are reserved for the exact endpoints; the rest pair up as
       one (min, max) per bucket. *)
    let buckets = (max_points - 2) / 2 in
    let interior = n - 2 in
    let bucket_bounds index =
      (* Even partition of indices [1 .. n - 2] into [buckets] pieces. *)
      let start = 1 + (interior * index / buckets) in
      let stop = 1 + (interior * (index + 1) / buckets) in
      start, stop
    in
    let bucket_extremes index =
      let start, stop = bucket_bounds index in
      match start >= stop with
      | true -> []
      | false ->
        let min_i = ref start
        and max_i = ref start in
        for i = start + 1 to stop - 1 do
          let (_ : float), y = arr.(i) in
          if Float.O.(y < snd arr.(!min_i)) then min_i := i;
          if Float.O.(y > snd arr.(!max_i)) then max_i := i
        done;
        (match Int.min !min_i !max_i = Int.max !min_i !max_i with
         | true -> [ arr.(!min_i) ]
         | false ->
           [ arr.(Int.min !min_i !max_i); arr.(Int.max !min_i !max_i) ])
    in
    List.concat
      [ [ arr.(0) ]
      ; List.concat_map (List.init buckets ~f:Fn.id) ~f:bucket_extremes
      ; [ arr.(n - 1) ]
      ]
;;
