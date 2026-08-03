open! Core
open Types

module Compiled_rule = struct
  type t =
    { rule : Rule.t
    ; every_ticks : int option
    }
end

type t = { rules : Compiled_rule.t list }

let rules t = t.rules

(* [span / tick] when that is exact, an error otherwise. [what] labels the
   span in errors, e.g. "EVERY interval". *)
let ticks_of_span span ~tick ~what =
  let span_ns = Time_ns.Span.to_int_ns span in
  let tick_ns = Time_ns.Span.to_int_ns tick in
  match span_ns % tick_ns = 0 with
  | true -> Ok (span_ns / tick_ns)
  | false ->
    Or_error.error_s
      [%message
        "span is not a whole number of simulation ticks"
          (what : string)
          (span : Time_ns.Span.t)
          (tick : Time_ns.Span.t)]
;;

let validate_every (rule : Rule.t) ~tick =
  match rule.every with
  | None -> Ok None
  | Some span ->
    (match ticks_of_span span ~tick ~what:"EVERY interval" with
     | Error _ as error -> error
     | Ok ticks -> Ok (Some ticks))
;;

let validate_window (window : Market_signal.Window.t) ~tick =
  let open Or_error.Let_syntax in
  let non_negative what span =
    match Time_ns.Span.( >= ) span Time_ns.Span.zero with
    | true -> Ok ()
    | false ->
      Or_error.error_s
        [%message
          "signal window offset is negative"
            (what : string)
            (span : Time_ns.Span.t)]
  in
  let%bind () = non_negative "start_ago" window.start_ago in
  let%bind () = non_negative "end_ago" window.end_ago in
  let%bind (_ : int) =
    ticks_of_span window.start_ago ~tick ~what:"signal window start_ago"
  in
  let%bind (_ : int) =
    ticks_of_span window.end_ago ~tick ~what:"signal window end_ago"
  in
  match Time_ns.Span.( >= ) window.start_ago window.end_ago with
  | true -> Ok ()
  | false ->
    Or_error.error_s
      [%message
        "signal window starts after it ends"
          ~start_ago:(window.start_ago : Time_ns.Span.t)
          ~end_ago:(window.end_ago : Time_ns.Span.t)]
;;

let validate_signals (rule : Rule.t) ~tick =
  List.map (Rule.signals rule) ~f:(fun signal ->
    match (signal : Market_signal.t) with
    | Moved { window; _ } -> validate_window window ~tick
    | Above _ | Below _ -> Ok ())
  |> Or_error.combine_errors_unit
;;

let validate_slugs (rule : Rule.t) ~known_slugs =
  List.map (Rule.referenced_slugs rule) ~f:(fun slug ->
    match Hash_set.mem known_slugs slug with
    | true -> Ok ()
    | false ->
      Or_error.error_s
        [%message
          "rule references a market not in this simulation" (slug : Slug.t)])
  |> Or_error.combine_errors_unit
;;

let create ~rules ~tick ~slugs =
  let open Or_error.Let_syntax in
  let%bind () =
    match Time_ns.Span.( > ) tick Time_ns.Span.zero with
    | true -> Ok ()
    | false ->
      Or_error.error_s
        [%message "tick span must be positive" (tick : Time_ns.Span.t)]
  in
  let known_slugs = Slug.Hash_set.of_list slugs in
  let%bind compiled =
    List.map rules ~f:(fun rule ->
      let%bind every_ticks = validate_every rule ~tick in
      let%bind () = validate_signals rule ~tick in
      let%bind () = validate_slugs rule ~known_slugs in
      return { Compiled_rule.rule; every_ticks })
    |> Or_error.combine_errors
  in
  return { rules = compiled }
;;
