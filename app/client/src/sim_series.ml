open! Core
open Types

module Series = struct
  type t =
    { name : string
    ; points : (float * float) list
    }
  [@@deriving sexp_of]
end

type t =
  { pnl : Series.t list
  ; value : Series.t list
  ; prices : Series.t list
  ; inventory : Series.t list
  ; baseline_pnl : Series.t list
  ; baseline_value : Series.t list
  }
[@@deriving sexp_of]

let create (result : Protocol.Sim_result.t) ~max_points =
  let series name points =
    { Series.name; points = Downsample.downsample points ~max_points }
  in
  (* Portfolio value once per tick, shared by two series. *)
  let ticks =
    List.map result.ticks ~f:(fun (tick : Protocol.Tick_point.t) ->
      ( tick
      , Portfolio.value ~inventory:tick.inventory ~yes_prices:tick.yes_prices
      ))
  in
  let tick_series name f =
    series
      name
      (List.map ticks ~f:(fun ((tick : Protocol.Tick_point.t), portfolio) ->
         tick.time_s, f tick portfolio))
  in
  let slugs =
    List.concat_map result.ticks ~f:(fun tick ->
      List.map tick.yes_prices ~f:fst)
    |> List.dedup_and_sort ~compare:Slug.compare
  in
  let per_slug field =
    List.map slugs ~f:(fun slug ->
      series
        (Slug.to_string slug)
        (List.filter_map result.ticks ~f:(fun tick ->
           List.Assoc.find (field tick) slug ~equal:Slug.equal
           |> Option.map ~f:(fun value ->
             tick.Protocol.Tick_point.time_s, value))))
  in
  let baseline name f =
    series
      name
      (List.map
         result.baseline_ticks
         ~f:(fun (point : Protocol.Baseline_point.t) ->
           point.time_s, f point))
  in
  let baseline_pnl, baseline_value =
    match result.baseline_ticks with
    | [] -> [], []
    | _ :: _ ->
      ( [ baseline "avg realized" (fun point -> point.realized)
        ; baseline "avg unrealized" (fun point -> point.unrealized)
        ; baseline "avg total" (fun point ->
            point.realized +. point.unrealized)
        ]
      , [ baseline "avg cash" (fun point -> point.cash)
        ; baseline "avg portfolio" (fun point -> point.portfolio_value)
        ; baseline "avg total value" (fun point ->
            point.cash +. point.portfolio_value)
        ] )
  in
  { pnl =
      [ tick_series "realized" (fun tick (_ : float) -> tick.realized)
      ; tick_series "unrealized" (fun tick (_ : float) -> tick.unrealized)
      ; tick_series "total" (fun tick (_ : float) ->
          tick.realized +. tick.unrealized)
      ]
  ; value =
      [ tick_series "cash" (fun tick (_ : float) -> tick.cash)
      ; tick_series "portfolio value" (fun (_ : Protocol.Tick_point.t) p ->
          p)
      ; tick_series "total value" (fun tick p -> tick.cash +. p)
      ]
  ; prices = per_slug (fun tick -> tick.yes_prices)
  ; inventory =
      List.map slugs ~f:(fun slug ->
        series
          (Slug.to_string slug)
          (List.filter_map result.ticks ~f:(fun tick ->
             List.Assoc.find tick.inventory slug ~equal:Slug.equal
             |> Option.map ~f:(fun count -> tick.time_s, Float.of_int count))))
  ; baseline_pnl
  ; baseline_value
  }
;;
