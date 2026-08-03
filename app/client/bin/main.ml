(* The web app's client half: a Bonsai single-page app served by
   [app/server]. The Markets page renders the category-grouped market
   browser; the Bots page is a three-stage bot builder — pick markets (search
   with autocomplete), write rules (editor with autocomplete and window
   controls), run and study the results (fills, pnl and price charts). Market
   data arrives over {!Protocol.get_markets}; backtests run server-side via
   {!Protocol.run_simulation}. *)

open! Core
open Types
open Bonsai_web
open Bonsai.Let_syntax
module Card = Protocol.Market_card

let max_per_category = 5
let min_per_category = 3
let max_markets = 4

(* A real stylesheet (injected as a <style> node) rather than inline styles,
   so hover states and shared classes work. *)
let css =
  {|
  body {
    margin: 0;
    background: radial-gradient(1200px 600px at 70% -10%, #1a2340, #0b0e14 60%);
    min-height: 100vh;
    color: #e8eaf0;
    font-family: "Segoe UI", system-ui, sans-serif;
  }
  .header { padding: 28px 32px 0; }
  .title {
    margin: 0;
    font-size: 34px;
    font-weight: 800;
    letter-spacing: 0.5px;
    background: linear-gradient(90deg, #7dd3fc, #c084fc);
    -webkit-background-clip: text;
    background-clip: text;
    color: transparent;
  }
  .subtitle { color: #8b93a7; margin: 4px 0 20px; font-size: 14px; }
  .nav { display: flex; gap: 10px; padding: 0 32px 26px; }
  .nav-button {
    border: 1px solid #2a3040;
    background: #141926;
    color: #aeb6c8;
    padding: 8px 18px;
    border-radius: 999px;
    cursor: pointer;
    font-size: 14px;
    transition: border-color 0.15s, color 0.15s;
  }
  .nav-button:hover { border-color: #7dd3fc; color: #e8eaf0; }
  .nav-button-active {
    background: linear-gradient(90deg, #0ea5e9, #8b5cf6);
    color: white;
    border-color: transparent;
  }
  .page { padding: 0 32px 48px; }
  .status { color: #8b93a7; }
  .category-section { margin-bottom: 34px; }
  .category-header {
    display: flex;
    align-items: center;
    gap: 10px;
    margin-bottom: 14px;
  }
  .category-dot { width: 10px; height: 10px; border-radius: 50%; }
  .category-name { margin: 0; font-size: 20px; font-weight: 700; }
  .card-row { display: flex; flex-wrap: wrap; gap: 14px; }
  .card {
    background: #141926;
    border: 1px solid #232a3b;
    border-radius: 12px;
    padding: 14px 16px;
    width: 240px;
    display: flex;
    flex-direction: column;
    gap: 8px;
    transition: transform 0.12s, border-color 0.12s;
  }
  .card:hover { transform: translateY(-3px); border-color: #3b82f6; }
  .card-title { font-weight: 600; font-size: 14px; line-height: 1.35; }
  .card-slug {
    color: #606a82;
    font-size: 11px;
    font-family: monospace;
    overflow-wrap: anywhere;
  }
  .volume-badge {
    align-self: flex-start;
    background: #0f2e1e;
    color: #4ade80;
    font-size: 12px;
    padding: 2px 10px;
    border-radius: 999px;
  }
  .stage-title { margin: 0 0 6px; font-size: 22px; }
  .stage-hint { color: #8b93a7; font-size: 13px; margin: 0 0 16px; }
  .search-input, .num-input, .interval-select {
    background: #10151f;
    border: 1px solid #2a3040;
    border-radius: 8px;
    color: #e8eaf0;
    padding: 10px 14px;
    font-size: 14px;
    width: 420px;
  }
  .search-input:focus { outline: none; border-color: #7dd3fc; }
  .num-input { width: 90px; }
  .interval-select { width: auto; }
  .suggestions {
    width: 420px;
    background: #10151f;
    border: 1px solid #2a3040;
    border-radius: 8px;
    margin-top: 6px;
    overflow: hidden;
  }
  .suggestion {
    display: block;
    width: 100%;
    text-align: left;
    background: none;
    border: none;
    border-bottom: 1px solid #1c2331;
    color: #cdd4e2;
    padding: 8px 14px;
    font-size: 13px;
    cursor: pointer;
  }
  .suggestion:hover { background: #1a2233; color: white; }
  .chips { display: flex; flex-wrap: wrap; gap: 8px; margin: 14px 0; }
  .chip {
    display: flex;
    align-items: center;
    gap: 8px;
    background: #172033;
    border: 1px solid #2c3a55;
    border-radius: 999px;
    padding: 5px 12px;
    font-size: 13px;
  }
  .chip-remove {
    background: none;
    border: none;
    color: #8b93a7;
    cursor: pointer;
    font-size: 14px;
    padding: 0;
  }
  .chip-remove:hover { color: #f87171; }
  .editor {
    background: #10151f;
    border: 1px solid #2a3040;
    border-radius: 8px;
    color: #e8eaf0;
    font-family: monospace;
    font-size: 14px;
    padding: 12px 14px;
    width: 620px;
    height: 180px;
    resize: vertical;
  }
  .editor:focus { outline: none; border-color: #7dd3fc; }
  .controls { display: flex; gap: 18px; align-items: end; margin: 16px 0; }
  .control-label { display: block; color: #8b93a7; font-size: 12px; margin-bottom: 4px; }
  .btn-primary, .btn-secondary {
    border: none;
    border-radius: 8px;
    padding: 10px 22px;
    font-size: 14px;
    cursor: pointer;
  }
  .btn-primary {
    background: linear-gradient(90deg, #0ea5e9, #8b5cf6);
    color: white;
  }
  .btn-primary:disabled { opacity: 0.4; cursor: default; }
  .btn-secondary {
    background: #141926;
    border: 1px solid #2a3040;
    color: #aeb6c8;
  }
  .button-row { display: flex; gap: 10px; margin-top: 10px; }
  .error-box {
    background: #2b1520;
    border: 1px solid #7f1d1d;
    border-radius: 8px;
    color: #fca5a5;
    font-family: monospace;
    font-size: 13px;
    padding: 12px 14px;
    margin: 12px 0;
    white-space: pre-wrap;
    max-width: 620px;
  }
  .chart-box {
    background: #10151f;
    border: 1px solid #232a3b;
    border-radius: 12px;
    padding: 16px;
    margin: 0 0 20px;
    width: fit-content;
  }
  .chart-title { margin: 0 0 8px; font-size: 15px; }
  .legend { display: flex; gap: 14px; font-size: 12px; color: #8b93a7; margin-bottom: 8px; }
  .legend-item { display: flex; align-items: center; gap: 6px; }
  .legend-dot { width: 8px; height: 8px; border-radius: 50%; }
  .fills-table { border-collapse: collapse; font-size: 13px; margin: 0 0 24px; }
  .fills-table th {
    text-align: left;
    color: #8b93a7;
    font-weight: 500;
    padding: 6px 14px 6px 0;
    border-bottom: 1px solid #2a3040;
  }
  .fills-table td { padding: 6px 14px 6px 0; border-bottom: 1px solid #1c2331; }
  .fill-accepted { color: #4ade80; }
  .fill-rejected { color: #f87171; }
  .final-book { font-size: 15px; margin: 10px 0 20px; }
|}
;;

module Page = struct
  type t =
    | Markets
    | Bots
  [@@deriving sexp_of, compare, equal, enumerate]

  let name = function Markets -> "Markets" | Bots -> "Bots"
end

let category_color : Category.t -> string = function
  | Elections -> "#c084fc"
  | Politics -> "#f472b6"
  | Sports -> "#4ade80"
  | Culture -> "#fbbf24"
  | Crypto -> "#f97316"
  | Commodities -> "#eab308"
  | Climate -> "#34d399"
  | Economy -> "#60a5fa"
  | Mentions -> "#a78bfa"
  | Finance -> "#38bdf8"
  | Tech -> "#22d3ee"
  | Miscellaneous -> "#94a3b8"
;;

let series_palette = [ "#7dd3fc"; "#c084fc"; "#4ade80"; "#fbbf24" ]

let series_color index =
  List.nth series_palette (index % List.length series_palette)
  |> Option.value ~default:"#7dd3fc"
;;

let style text = Vdom.Attr.create "style" text
let cls name = Vdom.Attr.class_ name

let on_click effect =
  Vdom.Attr.on_click
    (fun (_ : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t) -> effect)
;;

let button ?(enabled = true) ~class_ ~label effect =
  Vdom.Node.button
    ~attrs:
      ([ cls class_; on_click effect ]
       @ if enabled then [] else [ Vdom.Attr.disabled ])
    [ Vdom.Node.text label ]
;;

(* ---------- Markets page ---------- *)

let card_view (card : Card.t) =
  let volume =
    match card.volume with
    | None -> "volume unreported"
    | Some (Contracts contracts) ->
      let count = Int.to_string_hum ~delimiter:',' (Size.to_int contracts) in
      [%string "%{count} contracts"]
    | Some (Notional dollars) ->
      [%string "%{Price.to_string_dollar dollars} traded"]
  in
  Vdom.Node.div
    ~attrs:[ cls "card" ]
    [ Vdom.Node.div ~attrs:[ cls "card-title" ] [ Vdom.Node.text card.title ]
    ; Vdom.Node.div
        ~attrs:[ cls "card-slug" ]
        [ Vdom.Node.text (Slug.to_string card.slug) ]
    ; Vdom.Node.div ~attrs:[ cls "volume-badge" ] [ Vdom.Node.text volume ]
    ]
;;

let category_section (category, cards) =
  Vdom.Node.div
    ~attrs:[ cls "category-section" ]
    [ Vdom.Node.div
        ~attrs:[ cls "category-header" ]
        [ Vdom.Node.div
            ~attrs:
              [ cls "category-dot"
              ; style [%string "background:%{category_color category}"]
              ]
            []
        ; Vdom.Node.h2
            ~attrs:[ cls "category-name" ]
            [ Vdom.Node.text (Category.to_string category) ]
        ]
    ; Vdom.Node.div ~attrs:[ cls "card-row" ] (List.map cards ~f:card_view)
    ]
;;

let markets_view cards =
  match
    Client_logic.Market_groups.group
      cards
      ~max_per_category
      ~min_per_category
  with
  | [] ->
    Vdom.Node.div
      ~attrs:[ cls "status" ]
      [ Vdom.Node.text "no category has enough markets to show" ]
  | groups -> Vdom.Node.div (List.map groups ~f:category_section)
;;

let markets_page_view markets =
  match markets with
  | None ->
    Vdom.Node.div
      ~attrs:[ cls "status" ]
      [ Vdom.Node.text "loading markets..." ]
  | Some (Error error) ->
    Vdom.Node.pre [ Vdom.Node.text (Error.to_string_hum error) ]
  | Some (Ok cards) -> markets_view cards
;;

(* ---------- Bots page ---------- *)

module Stage = struct
  type t =
    | Pick
    | Rules
    | Results
  [@@deriving sexp_of, equal]
end

module Sim_state = struct
  type t =
    | Idle
    | Running
    | Done of Protocol.Sim_result.t
  [@@deriving sexp_of]
end

let suggestion_list suggestions ~on_pick =
  match suggestions with
  | [] -> Vdom.Node.none
  | suggestions ->
    Vdom.Node.div
      ~attrs:[ cls "suggestions" ]
      (List.map suggestions ~f:(fun suggestion ->
         Vdom.Node.button
           ~attrs:[ cls "suggestion"; on_click (on_pick suggestion) ]
           [ Vdom.Node.text suggestion ]))
;;

let chips selected ~on_remove =
  Vdom.Node.div
    ~attrs:[ cls "chips" ]
    (List.map selected ~f:(fun (card : Card.t) ->
       Vdom.Node.div
         ~attrs:[ cls "chip" ]
         [ Vdom.Node.text (Slug.to_string card.slug)
         ; Vdom.Node.button
             ~attrs:[ cls "chip-remove"; on_click (on_remove card) ]
             [ Vdom.Node.text "✕" ]
         ]))
;;

let pick_view
  ~all_cards
  ~selected
  ~set_selected
  ~search_text
  ~set_search_text
  ~continue
  =
  let index_cards =
    List.filter all_cards ~f:(fun (card : Card.t) ->
      not
        (List.mem selected card ~equal:(fun a b ->
           Slug.equal a.Card.slug b.Card.slug)))
  in
  let suggestions =
    Autocomplete.suggest
      (Client_logic.Search_index.env index_cards)
      ~input:search_text
  in
  let pick text =
    match Client_logic.Search_index.find index_cards ~text with
    | None -> Effect.Ignore
    | Some card ->
      (match List.length selected < max_markets with
       | false -> Effect.Ignore
       | true ->
         Effect.Many
           [ set_selected (selected @ [ card ]); set_search_text "" ])
  in
  let remove (card : Card.t) =
    set_selected
      (List.filter selected ~f:(fun kept ->
         not (Slug.equal kept.Card.slug card.slug)))
  in
  let count = List.length selected in
  Vdom.Node.div
    [ Vdom.Node.h2
        ~attrs:[ cls "stage-title" ]
        [ Vdom.Node.text "Pick markets" ]
    ; Vdom.Node.p
        ~attrs:[ cls "stage-hint" ]
        [ Vdom.Node.text
            [%string
              "search by title or ticker and select 1 to %{max_markets#Int} \
               markets (%{count#Int} selected)"]
        ]
    ; Vdom.Node.input
        ~attrs:
          [ cls "search-input"
          ; Vdom.Attr.placeholder "search markets..."
          ; Vdom.Attr.value search_text
          ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
              set_search_text text)
          ]
        ()
    ; suggestion_list suggestions ~on_pick:pick
    ; chips selected ~on_remove:remove
    ; button
        ~enabled:(count >= 1 && count <= max_markets)
        ~class_:"btn-primary"
        ~label:"Continue to rules"
        continue
    ]
;;

let rules_view
  ~selected
  ~program
  ~set_program
  ~interval
  ~set_interval
  ~lookback
  ~set_lookback
  ~warmup
  ~set_warmup
  ~error
  ~back
  ~run
  =
  let tickers = List.map selected ~f:(fun (card : Card.t) -> card.slug) in
  let token = Client_logic.Rule_candidates.current_token program in
  let suggestions =
    match String.is_empty token with
    | true -> []
    | false ->
      Autocomplete.suggest
        (Client_logic.Rule_candidates.env ~tickers ~program)
        ~input:token
  in
  let insert suggestion =
    set_program
      (Client_logic.Rule_candidates.complete program ~with_:suggestion)
  in
  let numbers_valid =
    Option.is_some (Int.of_string_opt lookback)
    && Option.is_some (Int.of_string_opt warmup)
  in
  let interval_option value =
    Vdom.Node.option
      ~attrs:
        ([ Vdom.Attr.value (Protocol.Interval.name value) ]
         @
         match Protocol.Interval.equal value interval with
         | true -> [ Vdom.Attr.selected ]
         | false -> [])
      [ Vdom.Node.text (Protocol.Interval.name value) ]
  in
  Vdom.Node.div
    [ Vdom.Node.h2
        ~attrs:[ cls "stage-title" ]
        [ Vdom.Node.text "Write rules" ]
    ; Vdom.Node.p
        ~attrs:[ cls "stage-hint" ]
        [ Vdom.Node.text
            "one statement per line; click a suggestion to complete the \
             word you are typing"
        ]
    ; chips selected ~on_remove:(fun (_ : Card.t) -> Effect.Ignore)
    ; Vdom.Node.textarea
        ~attrs:
          [ cls "editor"
          ; Vdom.Attr.placeholder "every 2h buy 1 <ticker> yes"
          ; Vdom.Attr.value program
          ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
              set_program text)
          ]
        []
    ; suggestion_list suggestions ~on_pick:insert
    ; (match error with
       | None -> Vdom.Node.none
       | Some error ->
         Vdom.Node.div
           ~attrs:[ cls "error-box" ]
           [ Vdom.Node.text (Error.to_string_hum error) ])
    ; Vdom.Node.div
        ~attrs:[ cls "controls" ]
        [ Vdom.Node.div
            [ Vdom.Node.label
                ~attrs:[ cls "control-label" ]
                [ Vdom.Node.text "interval" ]
            ; Vdom.Node.select
                ~attrs:
                  [ cls "interval-select"
                  ; Vdom.Attr.on_change (fun (_ : _ Js_of_ocaml.Js.t) name ->
                      match
                        List.find Protocol.Interval.all ~f:(fun value ->
                          String.equal (Protocol.Interval.name value) name)
                      with
                      | None -> Effect.Ignore
                      | Some value -> set_interval value)
                  ]
                (List.map Protocol.Interval.all ~f:interval_option)
            ]
        ; Vdom.Node.div
            [ Vdom.Node.label
                ~attrs:[ cls "control-label" ]
                [ Vdom.Node.text "lookback (days)" ]
            ; Vdom.Node.input
                ~attrs:
                  [ cls "num-input"
                  ; Vdom.Attr.value lookback
                  ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
                      set_lookback text)
                  ]
                ()
            ]
        ; Vdom.Node.div
            [ Vdom.Node.label
                ~attrs:[ cls "control-label" ]
                [ Vdom.Node.text "warmup (hours)" ]
            ; Vdom.Node.input
                ~attrs:
                  [ cls "num-input"
                  ; Vdom.Attr.value warmup
                  ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
                      set_warmup text)
                  ]
                ()
            ]
        ]
    ; Vdom.Node.div
        ~attrs:[ cls "button-row" ]
        [ button ~class_:"btn-secondary" ~label:"Back" back
        ; button
            ~enabled:
              ((not (String.is_empty (String.strip program)))
               && numbers_valid)
            ~class_:"btn-primary"
            ~label:"Run backtest"
            run
        ]
    ]
;;

let svg_node = Vdom.Node.create_svg

let chart_view ~title ~series ~sim_start_s =
  let width = 640. in
  let height = 220. in
  let all_points =
    List.concat_map series ~f:(fun (_, _, points) -> points)
  in
  let x_range =
    Client_logic.Chart.Range.of_values (List.map all_points ~f:fst)
  in
  let y_range =
    Client_logic.Chart.Range.of_values (List.map all_points ~f:snd)
  in
  let warmup_width =
    Client_logic.Chart.scale_x x_range ~extent:width sim_start_s
    |> Float.clamp_exn ~min:0. ~max:width
  in
  let polylines =
    List.map series ~f:(fun (_, color, points) ->
      svg_node
        "polyline"
        ~attrs:
          [ Vdom.Attr.create
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
      (List.map series ~f:(fun (name, color, _) ->
         Vdom.Node.div
           ~attrs:[ cls "legend-item" ]
           [ Vdom.Node.div
               ~attrs:
                 [ cls "legend-dot"; style [%string "background:%{color}"] ]
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
        ([ svg_node
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

let fills_table (fills : Protocol.Fill.t list) =
  match fills with
  | [] ->
    Vdom.Node.p
      ~attrs:[ cls "status" ]
      [ Vdom.Node.text "the bot made no trades" ]
  | fills ->
    let header text = Vdom.Node.th [ Vdom.Node.text text ] in
    let row (fill : Protocol.Fill.t) =
      let side = match fill.side with Buy -> "buy" | Sell -> "sell" in
      let contract = match fill.contract with Yes -> "yes" | No -> "no" in
      let time =
        Time_ns.Span.of_sec fill.time_s
        |> Time_ns.of_span_since_epoch
        |> Time_ns.to_string_utc
      in
      let status, status_class =
        match fill.rejected with
        | None -> "accepted", "fill-accepted"
        | Some reason -> [%string "REJECTED: %{reason}"], "fill-rejected"
      in
      Vdom.Node.tr
        [ Vdom.Node.td [ Vdom.Node.text time ]
        ; Vdom.Node.td
            [ Vdom.Node.text
                [%string
                  "#%{fill.id#Int} %{side} %{fill.size#Int} %{contract} on \
                   %{fill.slug#Slug}"]
            ]
        ; Vdom.Node.td ~attrs:[ cls status_class ] [ Vdom.Node.text status ]
        ]
    in
    Vdom.Node.table
      ~attrs:[ cls "fills-table" ]
      (Vdom.Node.tr [ header "time"; header "action"; header "status" ]
       :: List.map fills ~f:row)
;;

let results_view ~sim_state ~edit_rules ~new_bot =
  let body =
    match (sim_state : Sim_state.t) with
    | Idle ->
      Vdom.Node.p
        ~attrs:[ cls "status" ]
        [ Vdom.Node.text "nothing run yet" ]
    | Running ->
      Vdom.Node.p
        ~attrs:[ cls "status" ]
        [ Vdom.Node.text
            "running backtest — fetching live Kalshi history, this takes a \
             few seconds..."
        ]
    | Done result ->
      let pnl_series =
        [ ( "realized"
          , "#4ade80"
          , List.map result.ticks ~f:(fun tick -> tick.time_s, tick.realized)
          )
        ; ( "unrealized"
          , "#60a5fa"
          , List.map result.ticks ~f:(fun tick ->
              tick.time_s, tick.unrealized) )
        ; ( "total"
          , "#c084fc"
          , List.map result.ticks ~f:(fun tick ->
              tick.time_s, tick.realized +. tick.unrealized) )
        ]
      in
      let slugs =
        List.concat_map result.ticks ~f:(fun tick ->
          List.map tick.yes_prices ~f:fst)
        |> List.dedup_and_sort ~compare:Slug.compare
      in
      let price_series =
        List.mapi slugs ~f:(fun index slug ->
          ( Slug.to_string slug
          , series_color index
          , List.filter_map result.ticks ~f:(fun tick ->
              List.Assoc.find tick.yes_prices slug ~equal:Slug.equal
              |> Option.map ~f:(fun price -> tick.time_s, price)) ))
      in
      let final =
        match Protocol.Sim_result.final result with
        | None -> Vdom.Node.none
        | Some { cash; realized; unrealized; _ } ->
          Vdom.Node.p
            ~attrs:[ cls "final-book" ]
            [ Vdom.Node.text
                (sprintf
                   "final book: cash $%.2f | realized $%.2f | unrealized \
                    $%.2f"
                   cash
                   realized
                   unrealized)
            ]
      in
      Vdom.Node.div
        [ final
        ; chart_view
            ~title:"pnl (dollars, shaded region is warmup)"
            ~series:pnl_series
            ~sim_start_s:result.sim_start_s
        ; chart_view
            ~title:"market YES prices (dollars)"
            ~series:price_series
            ~sim_start_s:result.sim_start_s
        ; fills_table result.fills
        ]
  in
  Vdom.Node.div
    [ Vdom.Node.h2 ~attrs:[ cls "stage-title" ] [ Vdom.Node.text "Results" ]
    ; body
    ; Vdom.Node.div
        ~attrs:[ cls "button-row" ]
        [ button ~class_:"btn-secondary" ~label:"Edit rules" edit_rules
        ; button ~class_:"btn-secondary" ~label:"New bot" new_bot
        ]
    ]
;;

let bots_page markets_result (local_ graph) =
  let stage, set_stage = Bonsai.state Stage.Pick graph in
  let selected, set_selected = Bonsai.state ([] : Card.t list) graph in
  let search_text, set_search_text = Bonsai.state "" graph in
  let program, set_program = Bonsai.state "" graph in
  let interval, set_interval = Bonsai.state Protocol.Interval.Hour graph in
  let lookback, set_lookback = Bonsai.state "14" graph in
  let warmup, set_warmup = Bonsai.state "12" graph in
  let sim_state, set_sim_state = Bonsai.state Sim_state.Idle graph in
  let error, set_error = Bonsai.state (None : Error.t option) graph in
  let dispatch_sim =
    Rpc_effect.Rpc.dispatcher Protocol.run_simulation graph
  in
  let%arr stage
  and set_stage
  and selected
  and set_selected
  and search_text
  and set_search_text
  and program
  and set_program
  and interval
  and set_interval
  and lookback
  and set_lookback
  and warmup
  and set_warmup
  and sim_state
  and set_sim_state
  and error
  and set_error
  and dispatch_sim
  and markets_result in
  let all_cards =
    match markets_result with
    | Some (Ok cards) -> cards
    | Some (Error (_ : Error.t)) | None -> []
  in
  match (stage : Stage.t) with
  | Pick ->
    pick_view
      ~all_cards
      ~selected
      ~set_selected
      ~search_text
      ~set_search_text
      ~continue:(set_stage Rules)
  | Rules ->
    let run =
      match Int.of_string_opt lookback, Int.of_string_opt warmup with
      | Some lookback_days, Some warmup_hours ->
        let request : Protocol.Sim_request.t =
          { slugs = List.map selected ~f:(fun card -> card.Card.slug)
          ; program
          ; interval
          ; lookback_days
          ; warmup_hours
          }
        in
        let open Effect.Let_syntax in
        let%bind () =
          Effect.Many
            [ set_error None; set_sim_state Running; set_stage Results ]
        in
        let%bind response = dispatch_sim request in
        (match Or_error.join response with
         | Ok result -> set_sim_state (Done result)
         | Error error ->
           (* Bring the user back to their rules with the error inline. *)
           Effect.Many
             [ set_sim_state Idle; set_error (Some error); set_stage Rules ])
      | None, _ | _, None -> Effect.Ignore
    in
    rules_view
      ~selected
      ~program
      ~set_program
      ~interval
      ~set_interval
      ~lookback
      ~set_lookback
      ~warmup
      ~set_warmup
      ~error
      ~back:(set_stage Pick)
      ~run
  | Results ->
    results_view
      ~sim_state
      ~edit_rules:(set_stage Rules)
      ~new_bot:
        (Effect.Many
           [ set_selected []
           ; set_program ""
           ; set_sim_state Idle
           ; set_error None
           ; set_stage Pick
           ])
;;

(* ---------- App shell ---------- *)

let fetch_markets (local_ graph) =
  (* [where_to_connect] defaults to the serving host's websocket. *)
  let dispatch = Rpc_effect.Rpc.dispatcher Protocol.get_markets graph in
  let result, set_result = Bonsai.state_opt graph in
  let on_activate =
    let%map dispatch and set_result in
    let%bind.Effect response = dispatch () in
    set_result (Some (Or_error.join response))
  in
  Bonsai.Edge.lifecycle ~on_activate graph;
  result
;;

let nav_button ~current ~set_page page =
  let classes =
    match Page.equal page current with
    | true -> [ "nav-button"; "nav-button-active" ]
    | false -> [ "nav-button" ]
  in
  Vdom.Node.button
    ~attrs:[ Vdom.Attr.classes classes; on_click (set_page page) ]
    [ Vdom.Node.text (Page.name page) ]
;;

let app (local_ graph) =
  let page, set_page = Bonsai.state Page.Markets graph in
  let markets_result = fetch_markets graph in
  let bots = bots_page markets_result graph in
  let%arr page and set_page and markets_result and bots in
  let body =
    match page with
    | Markets -> markets_page_view markets_result
    | Bots -> bots
  in
  Vdom.Node.div
    [ Vdom.Node.create "style" [ Vdom.Node.text css ]
    ; Vdom.Node.div
        ~attrs:[ cls "header" ]
        [ Vdom.Node.h1 ~attrs:[ cls "title" ] [ Vdom.Node.text "Arbiter" ]
        ; Vdom.Node.p
            ~attrs:[ cls "subtitle" ]
            [ Vdom.Node.text
                "live Kalshi markets, grouped by category and ranked by \
                 volume — and a bot builder to trade them"
            ]
        ]
    ; Vdom.Node.div
        ~attrs:[ cls "nav" ]
        (List.map Page.all ~f:(nav_button ~current:page ~set_page))
    ; Vdom.Node.div ~attrs:[ cls "page" ] [ body ]
    ]
;;

let () = Bonsai_web.Start.start app
