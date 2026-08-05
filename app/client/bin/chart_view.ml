(* SVG line charts: the series palette, [Chart_series], and the
   [chart_view] renderer shared by bot results and the market detail
   popup. *)

open! Core
open! Bonsai_web
open Ui

let series_palette = [ "#7dd3fc"; "#c084fc"; "#4ade80"; "#fbbf24" ]

let series_color index =
  List.nth series_palette (index % List.length series_palette)
  |> Option.value ~default:"#7dd3fc"
;;

let svg_node = Vdom.Node.create_svg

(* One line on a chart. [dash] marks baseline (averaged dumb-bot) series:
   same hues as the configurable bot's lines, dashed stroke. *)
module Chart_series = struct
  type t =
    { name : string
    ; color : string
    ; dash : bool
    ; points : (float * float) list
    }
end

let solid ~name ~color points =
  { Chart_series.name; color; dash = false; points }
;;

let dashed ~name ~color points =
  { Chart_series.name; color; dash = true; points }
;;

(* [sim_start_s] shades everything before it (the warmup); omit it for charts
   with no warmup notion, like the market-detail popup. *)
let chart_view ~title ~(series : Chart_series.t list) ?sim_start_s () =
  let width = 640. in
  let height = 220. in
  let all_points =
    List.concat_map series ~f:(fun { Chart_series.points; _ } -> points)
  in
  let x_range =
    Client_logic.Chart.Range.of_values (List.map all_points ~f:fst)
  in
  let y_range =
    Client_logic.Chart.Range.of_values (List.map all_points ~f:snd)
  in
  let warmup_rect =
    match sim_start_s with
    | None -> []
    | Some sim_start_s ->
      let warmup_width =
        Client_logic.Chart.scale_x x_range ~extent:width sim_start_s
        |> Float.clamp_exn ~min:0. ~max:width
      in
      [ svg_node
          "rect"
          ~attrs:
            [ Vdom.Attr.create "x" "0"
            ; Vdom.Attr.create "y" "0"
            ; Vdom.Attr.create "width" (sprintf "%.1f" warmup_width)
            ; Vdom.Attr.create "height" (sprintf "%.0f" height)
            ; Vdom.Attr.create "fill" "#ffffff0a"
            ]
          []
      ]
  in
  let polylines =
    List.map series ~f:(fun { Chart_series.name = _; color; dash; points } ->
      svg_node
        "polyline"
        ~attrs:
          ([ Vdom.Attr.create
               "points"
               (Client_logic.Chart.polyline
                  points
                  ~x_range
                  ~y_range
                  ~width
                  ~height)
           ; Vdom.Attr.create "fill" "none"
           ; Vdom.Attr.create "stroke" color
           ; Vdom.Attr.create "stroke-width" "1.5"
           ]
           @
           match dash with
           | true -> [ Vdom.Attr.create "stroke-dasharray" "4 3" ]
           | false -> [])
        [])
  in
  let axis_label ~x ~y ~anchor text =
    svg_node
      "text"
      ~attrs:
        [ Vdom.Attr.create "x" (sprintf "%.0f" x)
        ; Vdom.Attr.create "y" (sprintf "%.0f" y)
        ; Vdom.Attr.create "fill" "#606a82"
        ; Vdom.Attr.create "font-size" "11"
        ; Vdom.Attr.create "text-anchor" anchor
        ]
      [ Vdom.Node.text text ]
  in
  let legend =
    Vdom.Node.div
      ~attrs:[ cls "legend" ]
      (List.map
         series
         ~f:(fun { Chart_series.name; color; dash = _; points = _ } ->
           Vdom.Node.div
             ~attrs:[ cls "legend-item" ]
             [ Vdom.Node.div
                 ~attrs:
                   [ cls "legend-dot"
                   ; style [%string "background:%{color}"]
                   ]
                 []
             ; Vdom.Node.text name
             ]))
  in
  Vdom.Node.div
    ~attrs:[ cls "chart-box" ]
    [ Vdom.Node.h3 ~attrs:[ cls "chart-title" ] [ Vdom.Node.text title ]
    ; legend
    ; svg_node
        "svg"
        ~attrs:
          [ Vdom.Attr.create "width" (sprintf "%.0f" width)
          ; Vdom.Attr.create "height" (sprintf "%.0f" height)
          ]
        (warmup_rect
         @ polylines
         @ [ axis_label
               ~x:2.
               ~y:12.
               ~anchor:"start"
               (Client_logic.Chart.label y_range.hi)
           ; axis_label
               ~x:2.
               ~y:(height -. 4.)
               ~anchor:"start"
               (Client_logic.Chart.label y_range.lo)
           ])
    ]
;;

