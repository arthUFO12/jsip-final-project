open! Core
open Types

module Interval = struct
  type t =
    | Minute
    | Hour
    | Day
  [@@deriving sexp]
end

module Point = struct
  type t =
    { time : Time_ns.t
    ; yes_price : Price.t
    ; no_price : Price.t
    }
  [@@deriving sexp_of]
end

type t = Point.t list

let rec advance ~(original : t) ~latest curr_time =
  match original with
  | point :: rest when Time_ns.( <= ) point.time curr_time ->
    advance ~original:rest ~latest:point curr_time
  | _ -> original, latest
;;

let rec interpolate_helper
  ~(original : t)
  ~interpolated
  ~curr_time
  ~finish
  ~time_step
  =
  if Time_ns.( > ) curr_time finish
  then Or_error.return interpolated
  else (
    let next_time = Time_ns.add curr_time time_step in
    match original with
    | [] -> Or_error.return interpolated
    | point :: rest ->
      if Time_ns.( > ) point.time curr_time
      then (
        match interpolated with
        | [] ->
          Or_error.error_string
            "start time is before the beginning of the data"
        | latest :: _ ->
          interpolate_helper
            ~original
            ~interpolated:
              (({ latest with time = curr_time } : Point.t) :: interpolated)
            ~curr_time:next_time
            ~finish
            ~time_step)
      else (
        let rest, latest = advance ~original:rest ~latest:point curr_time in
        interpolate_helper
          ~original:rest
          ~interpolated:({ latest with time = curr_time } :: interpolated)
          ~curr_time:next_time
          ~finish
          ~time_step))
;;

let interpolate (t : t) ~start ~finish ~interval : t Or_error.t =
  if Time_ns.( > ) start finish
  then
    Or_error.error_string
      [%string
        "start time is at the same time as or later than end time. Start: \
         %{start#Time_ns} End: %{finish#Time_ns}"]
  else (
    let initial_list = [] in
    let time_step =
      match (interval : Interval.t) with
      | Minute -> Time_ns.Span.of_min 1.
      | Hour -> Time_ns.Span.of_hr 1.
      | Day -> Time_ns.Span.of_day 1.
    in
    let interpolated =
      interpolate_helper
        ~original:t
        ~interpolated:initial_list
        ~curr_time:start
        ~finish
        ~time_step
    in
    Or_error.map interpolated ~f:List.rev)
;;

let shared_window series_by_stub =
  let open Or_error.Let_syntax in
  let%bind bounds =
    List.map series_by_stub ~f:(fun ((stub : Market_stub.t), series) ->
      match List.hd series, List.last series with
      | Some (first : Point.t), Some (last : Point.t) ->
        Ok (first.time, last.time)
      | _, _ ->
        Or_error.error_s
          [%message
            "no recent time series data" ~market:(stub.slug : Slug.t)])
    |> Or_error.combine_errors
  in
  let start =
    List.map bounds ~f:fst
    |> List.max_elt ~compare:Time_ns.compare
    |> Option.value_exn ~here:[%here]
  in
  let finish =
    List.map bounds ~f:snd
    |> List.min_elt ~compare:Time_ns.compare
    |> Option.value_exn ~here:[%here]
  in
  match Time_ns.( < ) start finish with
  | true -> Ok (start, finish)
  | false ->
    Or_error.error_s
      [%message
        "chosen markets have no overlapping data"
          (start : Time_ns.t)
          (finish : Time_ns.t)]
;;
