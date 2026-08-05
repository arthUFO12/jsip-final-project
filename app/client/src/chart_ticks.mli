(** Axis tick placement for {!Chart}: "nice" y values on a 1-2-5 ladder and
    UTC-labeled time ticks for the x axis. Pure — the Bonsai layer draws the
    gridlines and text nodes. *)

open! Core

(** At most [max_count] tick values inside the range, stepped by the smallest
    1-2-5 x 10^k step that fits, each an exact multiple of the step. Empty
    when the range is degenerate ([hi <= lo]). *)
val y_ticks : Chart.Range.t -> max_count:int -> float list

(** Time ticks for a range in epoch seconds: [(time_s, label)] pairs on a
    ladder of natural spans (hours, days, weeks), aligned to UTC multiples.
    Labels are ["Jul 9"] for day-or-longer steps and ["Jul 9 06:00"] below a
    day. Empty when the range is degenerate. *)
val time_ticks : Chart.Range.t -> max_count:int -> (float * string) list

(** The label {!time_ticks} would give a tick at [time_s] when stepping by
    [step] seconds — exposed for chart tooltips, which want the sub-day form
    (["Jul 9 06:00"], any [step] below 86,400) regardless of the axis's own
    step. *)
val time_label : step:float -> float -> string
