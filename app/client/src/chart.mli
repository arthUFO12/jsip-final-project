(** Pure SVG line-chart geometry: scaling data points into a pixel box and
    formatting axis labels. Returns strings and floats only — the Bonsai
    layer wraps them in svg nodes, and these functions expect-test natively.

    SVG's y axis grows downward, so [scale_y] inverts: [hi] maps to 0 and
    [lo] to [extent]. *)

open! Core

module Range : sig
  type t =
    { lo : float
    ; hi : float
    }

  (** Bounds of [values] padded by 5% so lines do not hug the frame; a
      degenerate (constant or empty) range is widened around its value. *)
  val of_values : float list -> t
end

val scale_x : Range.t -> extent:float -> float -> float
val scale_y : Range.t -> extent:float -> float -> float

(** Inverse of {!scale_x}: a pixel offset back to its data value, for
    mapping a hover position onto the series. *)
val unscale_x : Range.t -> extent:float -> float -> float

(** The point whose x is closest to [x], by binary search — [points] must
    be sorted by x ascending, as chart series are. [None] on empty. *)
val nearest : (float * float) array -> x:float -> (float * float) option

(** [(x, y)] data points as an SVG [polyline] [points] attribute within a
    [width] x [height] box. *)
val polyline
  :  (float * float) list
  -> x_range:Range.t
  -> y_range:Range.t
  -> width:float
  -> height:float
  -> string

(** Compact axis label: ["1.25"], ["-0.10"], ["120.00"]. *)
val label : float -> string
