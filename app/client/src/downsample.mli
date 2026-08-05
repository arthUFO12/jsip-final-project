(** Point-budget reduction for chart series, so a minute-interval month of
    ticks (~43k points) does not become a 43k-vertex SVG polyline. Min-max
    bucketing: the output keeps each bucket's lowest and highest point, so
    spikes survive where striding would erase them. Points are [(x, y)] in
    ascending x, as plotted by {!Chart.polyline}. *)

open! Core

(** [downsample points ~max_points] returns [points] unchanged when it has
    at most [max_points] elements. Otherwise the result keeps the first and
    last points exactly, plus each interior bucket's y-minimum and
    y-maximum in x order — never more than [max_points] points in total.
    Raises when [max_points < 4] (two endpoints + one min-max pair is the
    smallest useful budget). *)
val downsample
  :  (float * float) list
  -> max_points:int
  -> (float * float) list
