(* SVG line charts: responsive viewBox geometry, gridlines and 1-2-5 axis
   ticks, a shaded warmup band, and a hover layer — a crosshair snapped to
   the nearest x with one tooltip listing every series at that x. Series
   identity is a CSS class ([series-1] ..): the class sets [color] and the
   stylesheet's strokes and legend keys inherit it, so no hex lives here.

   Hover state lives with the caller (one cell per page section, keyed by
   chart title) so mousemove re-renders only this chart's overlay. *)

open! Core
open! Bonsai_web
open Ui

(* ViewBox geometry, shared with the design kit's chart card: a 640 x 260
   frame whose plot area leaves room for y labels left and time labels
   below. The svg renders at 100% width; hover math maps client pixels
   back through this box. *)
let view_w = 640.
let view_h = 260.
let plot_x = 40.
let plot_y = 8.
let plot_w = 592.
let plot_h = 224.
let y_tick_count = 4
let x_tick_count = 3

module Chart_series = struct
  type t =
    { name : string
    ; index : int (** 0-based; picks the [series-<n>] class, cycled past 5. *)
    ; dash : bool (** Dashed marks the averaged dumb-bot baseline. *)
    ; points : (float * float) list
    }
end

let series_class index = [%string "series-%{(index % 5) + 1#Int}"]

let solid ~name ~index points =
  { Chart_series.name; index; dash = false; points }
;;

let dashed ~name ~index points =
  { Chart_series.name; index; dash = true; points }
;;

let svg_node = Vdom.Node.create_svg
let float_attr name value = Vdom.Attr.create name (sprintf "%.1f" value)

let legend (series : Chart_series.t list) =
  match series with
  | [] | [ (_ : Chart_series.t) ] ->
    (* A single series needs no legend — the title names it. *)
    Vdom.Node.none
  | series ->
    Vdom.Node.div
      ~attrs:[ cls "legend" ]
      (List.map
         series
         ~f:(fun { Chart_series.name; index; dash; points = _ } ->
           Vdom.Node.div
             ~attrs:
               [ Vdom.Attr.classes [ "legend-item"; series_class index ] ]
             [ Vdom.Node.span
                 ~attrs:
                   [ Vdom.Attr.classes
                       ("legend-key" :: (if dash then [ "dashed" ] else []))
                   ]
                 []
             ; Vdom.Node.text name
             ]))
;;

let gridlines_and_ticks ~x_range ~y_range =
  let y_lines =
    Client_logic.Chart_ticks.y_ticks y_range ~max_count:y_tick_count
    |> List.concat_map ~f:(fun value ->
      let y =
        plot_y +. Client_logic.Chart.scale_y y_range ~extent:plot_h value
      in
      [ svg_node
          "line"
          ~attrs:
            [ cls "chart-gridline"
            ; float_attr "x1" plot_x
            ; float_attr "y1" y
            ; float_attr "x2" (plot_x +. plot_w)
            ; float_attr "y2" y
            ]
          []
      ; svg_node
          "text"
          ~attrs:
            [ cls "chart-tick"
            ; float_attr "x" (plot_x -. 6.)
            ; float_attr "y" (y +. 4.)
            ; Vdom.Attr.create "text-anchor" "end"
            ]
          [ Vdom.Node.text (Client_logic.Chart.label value) ]
      ])
  in
  let x_labels =
    Client_logic.Chart_ticks.time_ticks x_range ~max_count:x_tick_count
    |> List.map ~f:(fun (time_s, label) ->
      let x =
        plot_x +. Client_logic.Chart.scale_x x_range ~extent:plot_w time_s
      in
      svg_node
        "text"
        ~attrs:
          [ cls "chart-tick"
          ; float_attr "x" x
          ; float_attr "y" (view_h -. 8.)
          ; Vdom.Attr.create "text-anchor" "middle"
          ]
        [ Vdom.Node.text label ])
  in
  y_lines @ x_labels
;;

let warmup_band ~x_range sim_start_s =
  match sim_start_s with
  | None -> []
  | Some sim_start_s ->
    let width =
      Client_logic.Chart.scale_x x_range ~extent:plot_w sim_start_s
      |> Float.clamp_exn ~min:0. ~max:plot_w
    in
    [ svg_node
        "rect"
        ~attrs:
          [ cls "chart-warmup"
          ; float_attr "x" plot_x
          ; float_attr "y" plot_y
          ; float_attr "width" width
          ; float_attr "height" plot_h
          ]
        []
    ]
;;

let polylines ~x_range ~y_range (series : Chart_series.t list) =
  List.map
    series
    ~f:(fun { Chart_series.name = _; index; dash; points } ->
      svg_node
        "polyline"
        ~attrs:
          [ Vdom.Attr.classes
              ([ "chart-line"; series_class index ]
               @ if dash then [ "dashed" ] else [])
          ; Vdom.Attr.create
              "points"
              (Client_logic.Chart.polyline
                 points
                 ~x_range
                 ~y_range
                 ~width:plot_w
                 ~height:plot_h)
          ]
        [])
;;

(* The hover layer: a crosshair snapped to the nearest point's x, a dot
   per series, and one tooltip listing every series' value there — value
   first, name second, per the legend's inverted hierarchy. *)
let hover_overlay ~x_range ~y_range ~(series : Chart_series.t list) hover =
  match hover with
  | None -> [], Vdom.Node.none
  | Some data_x ->
    let arrays =
      List.map series ~f:(fun (s : Chart_series.t) ->
        s, Array.of_list s.points)
    in
    let snapped_x =
      (* Snap on the densest series so the crosshair always sits on data. *)
      List.max_elt arrays ~compare:(fun (_, a) (_, b) ->
        Int.compare (Array.length a) (Array.length b))
      |> Option.bind ~f:(fun ((_ : Chart_series.t), points) ->
        Client_logic.Chart.nearest points ~x:data_x)
      |> Option.map ~f:fst
    in
    (match snapped_x with
     | None -> [], Vdom.Node.none
     | Some snapped_x ->
       let x_px =
         plot_x
         +. Client_logic.Chart.scale_x x_range ~extent:plot_w snapped_x
       in
       let dots =
         List.filter_map arrays ~f:(fun (s, points) ->
           Client_logic.Chart.nearest points ~x:snapped_x
           |> Option.map ~f:(fun (px, py) -> s, px, py))
       in
       let crosshair =
         svg_node
           "line"
           ~attrs:
             [ cls "chart-crosshair"
             ; float_attr "x1" x_px
             ; float_attr "y1" plot_y
             ; float_attr "x2" x_px
             ; float_attr "y2" (plot_y +. plot_h)
             ]
           []
       in
       let dot_nodes =
         List.map dots ~f:(fun ((s : Chart_series.t), px, py) ->
           svg_node
             "circle"
             ~attrs:
               [ Vdom.Attr.classes
                   [ "chart-hover-dot"; series_class s.index ]
               ; float_attr
                   "cx"
                   (plot_x
                    +. Client_logic.Chart.scale_x x_range ~extent:plot_w px)
               ; float_attr
                   "cy"
                   (plot_y
                    +. Client_logic.Chart.scale_y y_range ~extent:plot_h py)
               ; Vdom.Attr.create "r" "3.5"
               ]
             [])
       in
       let tooltip =
         let percent = sprintf "%.1f" (x_px /. view_w *. 100.) in
         let position =
           (* Flip sides at the midline so the tooltip stays inside. *)
           match Float.O.(x_px /. view_w > 0.5) with
           | true ->
             [%string
               "left:%{percent}%; top:56px; transform:translateX(-105%)"]
           | false ->
             [%string
               "left:%{percent}%; top:56px; transform:translateX(8px)"]
         in
         Vdom.Node.div
           ~attrs:[ cls "chart-tooltip"; style position ]
           (Vdom.Node.div
              ~attrs:[ cls "chart-tooltip-time" ]
              [ Vdom.Node.text
                  (Client_logic.Chart_ticks.time_label
                     ~step:3600.
                     snapped_x)
              ]
            :: List.map
                 dots
                 ~f:(fun ((s : Chart_series.t), (_ : float), py) ->
                   Vdom.Node.div
                     ~attrs:
                       [ Vdom.Attr.classes
                           [ "chart-tooltip-row"; series_class s.index ]
                       ]
                     [ Vdom.Node.span ~attrs:[ cls "legend-key" ] []
                     ; Vdom.Node.span
                         ~attrs:[ cls "chart-tooltip-value" ]
                         [ Vdom.Node.text (Client_logic.Chart.label py) ]
                     ; Vdom.Node.span
                         ~attrs:[ cls "chart-tooltip-name" ]
                         [ Vdom.Node.text s.name ]
                     ]))
       in
       crosshair :: dot_nodes, tooltip)
;;

let on_chart_mousemove ~x_range ~set_hover =
  Vdom.Attr.on_mousemove (fun event ->
    let open Js_of_ocaml in
    match Js.Opt.to_option event##.currentTarget with
    | None -> Effect.Ignore
    | Some target ->
      let rect = target##getBoundingClientRect in
      let left = Js.to_float rect##.left in
      let width = Js.to_float rect##.right -. left in
      (match Float.O.(width > 0.) with
       | false -> Effect.Ignore
       | true ->
         let x_view =
           (Js.to_float event##.clientX -. left) /. width *. view_w
         in
         let x_plot =
           Float.clamp_exn (x_view -. plot_x) ~min:0. ~max:plot_w
         in
         set_hover
           (Some
              (Client_logic.Chart.unscale_x x_range ~extent:plot_w x_plot))))
;;

(* [sim_start_s] shades everything before it (the warmup); omit it for
   charts with no warmup notion, like the market-detail popup.

   [hover] is the caller's page-level hover cell — [(title, data_x)] —
   threaded through every chart on the page; each chart keys on its own
   title, so at most one tooltip shows and mousemove re-renders only the
   overlay of the hovered chart. *)
let chart_view
  ~title
  ~(series : Chart_series.t list)
  ?sim_start_s
  ~(hover : (string * float) option)
  ~set_hover
  ()
  =
  let hover =
    match hover with
    | Some (hovered_title, x) when String.equal hovered_title title ->
      Some x
    | Some ((_ : string), (_ : float)) | None -> None
  in
  let set_hover value =
    set_hover (Option.map value ~f:(fun x -> title, x))
  in
  let plottable =
    List.exists series ~f:(fun (s : Chart_series.t) ->
      List.length s.points >= 2)
  in
  let body =
    match plottable with
    | false ->
      [ Vdom.Node.div
          ~attrs:[ cls "chart-empty" ]
          [ Vdom.Node.text "not enough data to draw" ]
      ]
    | true ->
      let all_points =
        List.concat_map series ~f:(fun { Chart_series.points; _ } ->
          points)
      in
      let x_range =
        Client_logic.Chart.Range.of_values (List.map all_points ~f:fst)
      in
      let y_range =
        Client_logic.Chart.Range.of_values (List.map all_points ~f:snd)
      in
      let overlay_nodes, tooltip =
        hover_overlay ~x_range ~y_range ~series hover
      in
      [ svg_node
          "svg"
          ~attrs:
            [ cls "chart-svg"
            ; Vdom.Attr.create "viewBox" "0 0 640 260"
            ; on_chart_mousemove ~x_range ~set_hover
            ; Vdom.Attr.on_mouseleave
                (fun (_ : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t) ->
                  set_hover None)
            ]
          (warmup_band ~x_range sim_start_s
           @ gridlines_and_ticks ~x_range ~y_range
           @ polylines ~x_range ~y_range series
           @ overlay_nodes)
      ; tooltip
      ]
  in
  Vdom.Node.div
    ~attrs:[ cls "chart-box" ]
    (Vdom.Node.h3 ~attrs:[ cls "chart-title" ] [ Vdom.Node.text title ]
     :: legend series
     :: body)
;;
