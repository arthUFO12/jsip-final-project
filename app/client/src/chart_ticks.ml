open! Core

let multiples_within (range : Chart.Range.t) ~step =
  let first = Float.round_up (range.lo /. step) *. step in
  let count =
    (* Half-step slack so the top tick is not lost to float error. *)
    Float.iround_down_exn ((range.hi -. first +. (step /. 2.)) /. step) + 1
  in
  List.init (max 0 count) ~f:(fun i -> first +. (Float.of_int i *. step))
  |> List.filter ~f:(fun value ->
    Float.O.(value <= range.hi +. (step /. 100.)))
;;

let y_ticks (range : Chart.Range.t) ~max_count =
  match Float.O.(range.hi <= range.lo) || max_count < 1 with
  | true -> []
  | false ->
    let span = range.hi -. range.lo in
    let raw_step = span /. Float.of_int max_count in
    let magnitude = 10. ** Float.round_down (Float.log10 raw_step) in
    let step =
      (* The smallest 1-2-5 rung at or above the raw step. *)
      List.find_map [ 1.; 2.; 5.; 10. ] ~f:(fun rung ->
        let step = rung *. magnitude in
        match Float.O.(span /. step <= Float.of_int max_count) with
        | true -> Some step
        | false -> None)
      |> Option.value ~default:(10. *. magnitude)
    in
    multiples_within range ~step
;;

let hour = 3600.
let day = 24. *. hour

let time_steps =
  [ hour
  ; 3. *. hour
  ; 6. *. hour
  ; 12. *. hour
  ; day
  ; 2. *. day
  ; 7. *. day
  ; 14. *. day
  ; 30. *. day
  ]
;;

let time_label ~step time_s =
  let time = Time_ns.of_span_since_epoch (Time_ns.Span.of_sec time_s) in
  let date = Time_ns.to_date time ~zone:Timezone.utc in
  let day_part = [%string "%{Date.month date#Month} %{Date.day date#Int}"] in
  match Float.O.(step >= day) with
  | true -> day_part
  | false ->
    let ofday = Time_ns.to_ofday time ~zone:Timezone.utc in
    let parts = Time_ns.Ofday.to_parts ofday in
    [%string "%{day_part} %{sprintf \"%02d:%02d\" parts.hr parts.min}"]
;;

let time_ticks (range : Chart.Range.t) ~max_count =
  match Float.O.(range.hi <= range.lo) || max_count < 1 with
  | true -> []
  | false ->
    let span = range.hi -. range.lo in
    let step =
      List.find time_steps ~f:(fun step ->
        Float.O.(span /. step <= Float.of_int max_count))
      |> Option.value ~default:(30. *. day)
    in
    multiples_within range ~step
    |> List.map ~f:(fun time_s -> time_s, time_label ~step time_s)
;;
