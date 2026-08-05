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
  .arb-stages {
    display: flex; align-items: center; gap: 14px;
    margin: 4px 0 22px; flex-wrap: wrap;
  }
  .arb-stage {
    display: flex; align-items: center; gap: 12px;
    background: #141a26; border: 1px solid #2a3040; border-radius: 12px;
    padding: 12px 18px;
  }
  .arb-stage-number {
    width: 30px; height: 30px; border-radius: 50%;
    display: flex; align-items: center; justify-content: center;
    background: linear-gradient(90deg, #7dd3fc, #c084fc);
    color: #0b0e14; font-weight: 800;
  }
  .arb-stage-label { font-weight: 700; }
  .arb-stage-detail { font-size: 12px; color: #8b93a7; max-width: 220px; }
  .arb-stage-arrow { color: #4a5268; font-size: 20px; }
  .arb-panel {
    background: #10151f; border: 1px solid #232a3a; border-radius: 12px;
    padding: 16px 20px; margin-bottom: 18px;
  }
  .arb-panel-title { margin: 0 0 4px; font-size: 18px; }
  .arb-panel-hint { font-size: 13px; color: #8b93a7; margin: 0 0 12px; }
  .arb-controls { display: flex; align-items: center; gap: 12px; }
  .arb-summary { font-size: 14px; color: #a5b4d0; margin-top: 10px; }
  .arb-tabs { display: flex; gap: 8px; margin-bottom: 12px; }
  .arb-pair {
    border: 1px solid #232a3a; border-radius: 10px;
    padding: 10px 14px; margin-bottom: 8px;
  }
  .arb-pair-titles { display: flex; align-items: center; gap: 10px; flex-wrap: wrap; }
  .arb-venue {
    font-size: 11px; padding: 2px 8px; border-radius: 999px;
    border: 1px solid #2a3040; color: #8b93a7; white-space: nowrap;
  }
  .arb-score {
    font-family: ui-monospace, monospace; font-size: 12px; color: #7dd3fc;
    white-space: nowrap;
  }
  .arb-link { color: #4a5268; }
  .arb-llm { font-size: 12px; color: #8b93a7; margin-top: 6px; }
  .arb-pair-actions { display: flex; gap: 8px; margin-top: 8px; }
  .arb-edge {
    border: 1px solid #232a3a; border-radius: 10px;
    padding: 12px 14px; margin-bottom: 8px;
  }
  .arb-edge-tradable { border-color: #4ade80; }
  .arb-edge-legs { display: flex; gap: 18px; flex-wrap: wrap; }
  .arb-edge-leg { font-size: 13px; }
  .arb-outcome { font-weight: 700; color: #7dd3fc; margin-right: 6px; }
  .arb-edge-math { margin-top: 8px; font-family: ui-monospace, monospace; font-size: 13px; }
  .arb-edge-positive { color: #4ade80; }
  .arb-edge-negative { color: #f87171; }
  .arb-stage-click { cursor: pointer; transition: border-color 0.15s, box-shadow 0.15s; }
  .arb-stage-click:hover { border-color: #7dd3fc; }
  .arb-stage-active {
    border-color: #7dd3fc;
    box-shadow: 0 0 0 1px #7dd3fc inset;
  }
  .arb-edge-row { display: flex; align-items: baseline; gap: 10px; flex-wrap: wrap; }
  .arb-edge-row + .arb-edge-row { margin-top: 6px; }
  .arb-ask { margin-left: auto; font-family: ui-monospace, monospace; font-size: 13px; color: #a5b4d0; white-space: nowrap; }
  .arb-explainer {
    background: #141a26; border: 1px solid #2a3040; border-radius: 10px;
    padding: 12px 16px; margin: 12px 0; font-size: 13px; color: #a5b4d0;
  }
  .arb-explainer p { margin: 0 0 10px; }
  .arb-explainer p:last-child { margin-bottom: 0; }
  .arb-disclaimer { color: #fbbf24; }
  .arb-key-input {
    background: #141a26; border: 1px solid #2a3040; border-radius: 8px;
    color: #e8eaf0; padding: 7px 10px; font-family: ui-monospace, monospace;
    font-size: 13px; width: 340px; max-width: 100%;
  }
  .arb-trade-link {
    color: #7dd3fc; font-size: 12px; text-decoration: none;
    border: 1px solid #2a3040; border-radius: 999px; padding: 2px 10px;
    white-space: nowrap;
  }
  .arb-trade-link:hover { border-color: #7dd3fc; }
  .arb-wallet {
    background: #141a26; border: 1px solid #2a3040; border-radius: 10px;
    padding: 12px 16px; margin: 0 0 14px;
  }
  .arb-wallet-scores {
    display: flex; gap: 26px; flex-wrap: wrap; align-items: baseline;
    margin-bottom: 6px;
  }
  .arb-wallet-amount {
    font-family: ui-monospace, monospace; font-size: 22px; font-weight: 700;
  }
  .arb-wallet-paper { color: #c084fc; }
  .arb-wallet-real { color: #4ade80; }
  .arb-wallet-label { font-size: 12px; color: #8b93a7; }
  .arb-wallet-hint { font-size: 12px; color: #8b93a7; margin: 0 0 8px; }
  .arb-wallet-entry {
    display: flex; gap: 10px; align-items: baseline; font-size: 12px;
    color: #a5b4d0; border-top: 1px solid #1c2331; padding: 5px 0;
  }
  .arb-wallet-entry-dollars {
    font-family: ui-monospace, monospace; white-space: nowrap;
    margin-left: auto;
  }
  .arb-acted-badge {
    color: #4ade80; font-size: 11px; border: 1px solid #14532d;
    border-radius: 999px; padding: 1px 8px; white-space: nowrap;
  }
  .arb-edge-take {
    display: flex; gap: 12px; align-items: center; margin-top: 8px;
  }
  .arb-dialog {
    background: #141a26; border: 1px solid #7dd3fc; border-radius: 12px;
    padding: 16px 20px; margin: 0 0 14px;
  }
  .arb-onesided {
    background: #2b1520; border: 1px solid #7f1d1d; border-radius: 8px;
    color: #fca5a5; font-size: 13px; padding: 10px 14px; margin: 8px 0;
  }
|}
;;

module Page = struct
  type t =
    | Markets
    | Bots
    | Arbitrage
  [@@deriving sexp_of, compare, equal, enumerate]

  let name = function
    | Markets -> "Markets"
    | Bots -> "Bots"
    | Arbitrage -> "Arbitrage"
  ;;
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

(* ---------- Arbitrage page ---------- *)

module Sweep_state = struct
  type t =
    | Idle
    | Running
    | Done of Protocol.Sweep_summary.t
  [@@deriving sexp_of]
end

module Scan_state = struct
  type t =
    | Idle
    | Running
    | Done of Protocol.Scan_report.t
  [@@deriving sexp_of]
end

module Llm_state = struct
  type t =
    | Idle
    | Running
    | Done of Protocol.Llm_review_summary.t
    | Failed of string
  [@@deriving sexp_of]
end

module Auto_state = struct
  type t =
    | Idle
    | Running
    | Done of Protocol.Auto_review_summary.t
  [@@deriving sexp_of]
end

(* Assisted execution's two-step confirm: the human places the manual leg
   themselves, then the app hedges the Kalshi side. The nonce is minted when
   step 2 opens and reused for every retry of the same confirmation, so a
   timeout retry cannot double-hedge. *)
module Hedge_dialog = struct
  type t =
    | Closed
    | Step1 of
        { edge : Protocol.Edge_card.t
        ; preflight : Protocol.Preflight.t Or_error.t option
        }
    | Step2 of
        { edge : Protocol.Edge_card.t
        ; preflight : Protocol.Preflight.t
        ; nonce : string
        ; price_text : string
        ; count_text : string
        ; firing : bool
        }
    | Finished of
        { edge : Protocol.Edge_card.t
        ; result : Protocol.Hedge_result.t
        }
  [@@deriving sexp_of]
end

(* The leg the user must place themselves — whichever side isn't Kalshi. *)
let manual_leg (edge : Protocol.Edge_card.t) =
  match String.equal edge.yes.venue "Kalshi" with
  | true -> edge.no
  | false -> edge.yes
;;

let manual_is_yes (edge : Protocol.Edge_card.t) =
  not (String.equal edge.yes.venue "Kalshi")
;;

(* Cents of slack the hedge may pay over the preflight ask — shown to the
   user in step 2, enforced by the server's slippage guard. *)
let hedge_slippage_cents = 2

(* The three panels are separate sub-pages; this banner is both the pipeline
   overview and the way to swap between them. *)
module Arb_section = struct
  type t =
    | Sweep
    | Review
    | Pairs
  [@@deriving sexp_of, compare, equal, enumerate]

  let stage = function
    | Sweep -> "1", "Sweep", "scrape both venues and text-match every title"
    | Review ->
      "2", "Review", "LLM or human: approve pairs settling on the same event"
    | Pairs -> "3", "Arb Pairs", "price approved pairs on live order books"
  ;;
end

let arb_stage_banner ~current ~select =
  let stage section =
    let number, label, detail = Arb_section.stage section in
    let active =
      match Arb_section.equal section current with
      | true -> [ "arb-stage-active" ]
      | false -> []
    in
    Vdom.Node.div
      ~attrs:
        [ Vdom.Attr.classes ([ "arb-stage"; "arb-stage-click" ] @ active)
        ; on_click (select section)
        ]
      [ Vdom.Node.div
          ~attrs:[ cls "arb-stage-number" ]
          [ Vdom.Node.text number ]
      ; Vdom.Node.div
          [ Vdom.Node.div
              ~attrs:[ cls "arb-stage-label" ]
              [ Vdom.Node.text label ]
          ; Vdom.Node.div
              ~attrs:[ cls "arb-stage-detail" ]
              [ Vdom.Node.text detail ]
          ]
      ]
  in
  let arrow =
    Vdom.Node.div ~attrs:[ cls "arb-stage-arrow" ] [ Vdom.Node.text "→" ]
  in
  Vdom.Node.div
    ~attrs:[ cls "arb-stages" ]
    (List.map Arb_section.all ~f:stage |> List.intersperse ~sep:arrow)
;;

let arb_sweep_panel ~sweep_state ~threshold ~set_threshold ~run =
  let running =
    match (sweep_state : Sweep_state.t) with
    | Running -> true
    | Idle | Done (_ : Protocol.Sweep_summary.t) -> false
  in
  let status =
    match (sweep_state : Sweep_state.t) with
    | Idle -> Vdom.Node.div []
    | Running ->
      Vdom.Node.div
        ~attrs:[ cls "arb-summary" ]
        [ Vdom.Node.text
            "sweeping both venues' full listings — this pages through every \
             open market, give it a minute or two..."
        ]
    | Done { markets_swept; search_hits; proposed } ->
      Vdom.Node.div
        ~attrs:[ cls "arb-summary" ]
        [ Vdom.Node.text
            [%string
              "read %{markets_swept#Int} Kalshi and %{search_hits#Int} \
               Polymarket markets; filed %{proposed#Int} new pair(s) for \
               review"]
        ]
  in
  Vdom.Node.div
    ~attrs:[ cls "arb-panel" ]
    [ Vdom.Node.h2
        ~attrs:[ cls "arb-panel-title" ]
        [ Vdom.Node.text "1 · Sweep" ]
    ; Vdom.Node.p
        ~attrs:[ cls "arb-panel-hint" ]
        [ Vdom.Node.text
            "Compare every open market on both venues and keep title pairs \
             scoring above the threshold. Pure text matching — no LLM \
             credits are spent."
        ]
    ; Vdom.Node.div
        ~attrs:[ cls "arb-controls" ]
        [ Vdom.Node.label
            ~attrs:[ cls "control-label" ]
            [ Vdom.Node.text "threshold" ]
        ; Vdom.Node.input
            ~attrs:
              [ cls "num-input"
              ; Vdom.Attr.value threshold
              ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
                  set_threshold text)
              ]
            ()
        ; button
            ~enabled:(not running)
            ~class_:"btn-primary"
            ~label:"Run sweep"
            run
        ]
    ; status
    ]
;;

let arb_pair_row ~tab ~decide (pair : Protocol.Pair_card.t) =
  let side title venue =
    [ Vdom.Node.span [ Vdom.Node.text title ]
    ; Vdom.Node.span ~attrs:[ cls "arb-venue" ] [ Vdom.Node.text venue ]
    ]
  in
  let explanation =
    match pair.explanation with
    | None -> []
    | Some explanation ->
      [ Vdom.Node.div
          ~attrs:[ cls "arb-llm" ]
          [ Vdom.Node.text [%string "llm: %{explanation}"] ]
      ]
  in
  (* Every tab can re-file its pairs: Proposed is the normal gate, the other
     two are the undo path — grab a pair back out of a verdict the LLM (or a
     hasty click) got wrong. *)
  let action ~class_ ~label (new_status : Protocol.Pair_status.t) =
    button ~class_ ~label (decide ~index:pair.index ~new_status)
  in
  let actions =
    let buttons =
      match (tab : Protocol.Pair_status.t) with
      | Proposed ->
        [ action ~class_:"btn-primary" ~label:"Approve" Approved
        ; action ~class_:"btn-secondary" ~label:"Reject" Rejected
        ]
      | Approved ->
        [ action ~class_:"btn-secondary" ~label:"Reject" Rejected
        ; action ~class_:"btn-secondary" ~label:"Back to proposed" Proposed
        ]
      | Rejected ->
        [ action ~class_:"btn-primary" ~label:"Approve" Approved
        ; action ~class_:"btn-secondary" ~label:"Back to proposed" Proposed
        ]
    in
    [ Vdom.Node.div ~attrs:[ cls "arb-pair-actions" ] buttons ]
  in
  Vdom.Node.div
    ~attrs:[ cls "arb-pair" ]
    ([ Vdom.Node.div
         ~attrs:[ cls "arb-pair-titles" ]
         ([ Vdom.Node.span
              ~attrs:[ cls "arb-score" ]
              [ Vdom.Node.text (sprintf "%.2f" pair.score) ]
          ]
          @ side pair.left_title pair.left_venue
          @ [ Vdom.Node.span ~attrs:[ cls "arb-link" ] [ Vdom.Node.text "↔" ]
            ]
          @ side pair.right_title pair.right_venue)
     ]
     @ explanation
     @ actions)
;;

(* The free lever: a threshold rule instead of a judgment call. Its warning
   is the whole point — this approves on the text matcher's score alone, so
   it is exactly as trustworthy as the algorithm that filed the pairs. *)
let arb_auto_panel ~auto_state ~auto_threshold ~set_auto_threshold ~run =
  let running =
    match (auto_state : Auto_state.t) with
    | Running -> true
    | Idle | Done (_ : Protocol.Auto_review_summary.t) -> false
  in
  let status =
    match (auto_state : Auto_state.t) with
    | Idle -> Vdom.Node.div []
    | Running ->
      Vdom.Node.div
        ~attrs:[ cls "arb-summary" ]
        [ Vdom.Node.text "applying the threshold rule..." ]
    | Done { reviewed; approved; left_proposed } ->
      Vdom.Node.div
        ~attrs:[ cls "arb-summary" ]
        [ Vdom.Node.text
            [%string
              "read %{reviewed#Int} proposed pair(s): %{approved#Int} \
               auto-approved, %{left_proposed#Int} left proposed for a \
               closer look"]
        ]
  in
  Vdom.Node.div
    [ Vdom.Node.div
        ~attrs:[ cls "arb-explainer" ]
        [ Vdom.Node.p
            [ Vdom.Node.text
                "Prefer zero tokens? The deterministic gate approves every \
                 proposed pair whose text-match score clears the bar you \
                 set, instantly and for free."
            ]
        ; Vdom.Node.p
            ~attrs:[ cls "arb-disclaimer" ]
            [ Vdom.Node.text
                "⚠ No judgment happens here: this is only as viable as the \
                 text algorithm that proposed the pairs in the first place \
                 — it simply approves everything at or above the score. \
                 Similar titles are not identical settlements, so \
                 lookalikes that clear the bar get approved too, and a \
                 wrongly approved pair reads as an edge that isn't there. \
                 Spot-check the Approved tab, or pull mistakes back out \
                 below."
            ]
        ]
    ; Vdom.Node.div
        ~attrs:[ cls "arb-controls" ]
        [ Vdom.Node.label
            ~attrs:[ cls "control-label" ]
            [ Vdom.Node.text "min score" ]
        ; Vdom.Node.input
            ~attrs:
              [ cls "num-input"
              ; Vdom.Attr.value auto_threshold
              ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
                  set_auto_threshold text)
              ]
            ()
        ; button
            ~enabled:(not running)
            ~class_:"btn-secondary"
            ~label:"Auto-approve above score"
            run
        ]
    ; status
    ]
;;

(* The pitch, the hookup, and the fine print — the LLM is a first-pass
   analyst the user rents with their own key, never a silent cost. *)
let arb_llm_panel ~llm_state ~api_key ~set_api_key ~run =
  let running =
    match (llm_state : Llm_state.t) with
    | Running -> true
    | Idle | Done (_ : Protocol.Llm_review_summary.t) | Failed (_ : string)
      ->
      false
  in
  let status =
    match (llm_state : Llm_state.t) with
    | Idle -> Vdom.Node.div []
    | Running ->
      Vdom.Node.div
        ~attrs:[ cls "arb-summary" ]
        [ Vdom.Node.text
            "the analyst is reading the proposed queue — one pair per API \
             call, so a long queue takes a few minutes..."
        ]
    | Failed error ->
      Vdom.Node.div
        ~attrs:[ cls "arb-summary" ]
        [ Vdom.Node.text [%string "LLM review failed: %{error}"] ]
    | Done { reviewed; approved; rejected; errored; first_error } ->
      let errors =
        match first_error with
        | None -> ""
        | Some error ->
          [%string " · %{errored#Int} call(s) failed — first: %{error}"]
      in
      Vdom.Node.div
        ~attrs:[ cls "arb-summary" ]
        [ Vdom.Node.text
            [%string
              "read %{reviewed#Int} proposed pair(s): %{approved#Int} \
               approved, %{rejected#Int} rejected%{errors}"]
        ]
  in
  let paragraph text = Vdom.Node.p [ Vdom.Node.text text ] in
  Vdom.Node.div
    [ Vdom.Node.div
        ~attrs:[ cls "arb-explainer" ]
        [ paragraph
            "Here's the business: the same real-world question trades on \
             two venues under two names. The sweep reads every title on \
             Kalshi and Polymarket and files the lookalikes above a match \
             score. When YES over here plus NO over there costs less than \
             $1 all-in, you collect $1 whichever way the world goes — \
             that's the arb. The catch: \"lookalike\" is not \"identical\", \
             and only pairs that settle on exactly the same event are safe \
             to trade."
        ; paragraph
            "That judgment call is where your LLM comes in. Paste an \
             Anthropic API key and Claude reads the proposed queue like a \
             junior analyst who never sleeps: true twins land in Approved, \
             near-misses in Rejected, every verdict with a one-line \
             rationale you can audit below. You remain the desk head — any \
             pair can be pulled back out of either bucket with one click."
        ; Vdom.Node.p
            ~attrs:[ cls "arb-disclaimer" ]
            [ Vdom.Node.text
                "⚠ Tokens aren't free: every adjudication bills your key, \
                 and that spend comes out of the same pocket your edge pays \
                 into. Cents per pair times hundreds of pairs can eat a \
                 thin arb — sweep for free first, then spend tokens only on \
                 a queue worth judging."
            ]
        ]
    ; Vdom.Node.div
        ~attrs:[ cls "arb-controls" ]
        [ Vdom.Node.label
            ~attrs:[ cls "control-label" ]
            [ Vdom.Node.text "your Anthropic API key" ]
        ; Vdom.Node.input
            ~attrs:
              [ cls "arb-key-input"
              ; Vdom.Attr.type_ "password"
              ; Vdom.Attr.placeholder "sk-ant-... (blank = server's own key)"
              ; Vdom.Attr.value api_key
              ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
                  set_api_key text)
              ]
            ()
        ; button
            ~enabled:(not running)
            ~class_:"btn-primary"
            ~label:"LLM-review all proposed"
            run
        ]
    ; status
    ]
;;

let arb_review_panel ~tab ~pairs ~select_tab ~decide ~llm ~auto =
  let tab_button status =
    let class_ =
      match Protocol.Pair_status.equal status tab with
      | true -> "btn-primary"
      | false -> "btn-secondary"
    in
    button
      ~class_
      ~label:(Protocol.Pair_status.name status)
      (select_tab status)
  in
  let body =
    match (pairs : Protocol.Pair_card.t list Or_error.t option) with
    | None ->
      Vdom.Node.div ~attrs:[ cls "status" ] [ Vdom.Node.text "loading..." ]
    | Some (Error error) ->
      Vdom.Node.div
        ~attrs:[ cls "status" ]
        [ Vdom.Node.text (Error.to_string_hum error) ]
    | Some (Ok []) ->
      Vdom.Node.div
        ~attrs:[ cls "status" ]
        [ Vdom.Node.text
            [%string "no %{Protocol.Pair_status.name tab} pairs"]
        ]
    | Some (Ok pairs) ->
      Vdom.Node.div (List.map pairs ~f:(arb_pair_row ~tab ~decide))
  in
  Vdom.Node.div
    ~attrs:[ cls "arb-panel" ]
    [ Vdom.Node.h2
        ~attrs:[ cls "arb-panel-title" ]
        [ Vdom.Node.text "2 · Review" ]
    ; Vdom.Node.p
        ~attrs:[ cls "arb-panel-hint" ]
        [ Vdom.Node.text
            "The gate: only approve a pair if both markets settle on \
             exactly the same event — same threshold, same deadline. \
             Correlated is not identical. Judge by hand below, or rent the \
             LLM a seat at the desk."
        ]
    ; llm
    ; auto
    ; Vdom.Node.div
        ~attrs:[ cls "arb-tabs" ]
        (List.map Protocol.Pair_status.all ~f:tab_button)
    ; body
    ]
;;

let arb_edge_view
  ~mark_acted
  ~acted_keys
  ~assist
  (edge : Protocol.Edge_card.t)
  =
  let money value = sprintf "$%.2f" value in
  (* One row per leg, venue chip first: the two titles are the same event
     worded by different venues, so without the chips up front the card reads
     as one market listed twice. The link is the venue's own order book —
     where acting on the edge actually happens. *)
  let leg outcome (entry : Protocol.Edge_leg.t) =
    Vdom.Node.div
      ~attrs:[ cls "arb-edge-row" ]
      [ Vdom.Node.span
          ~attrs:[ cls "arb-outcome" ]
          [ Vdom.Node.text outcome ]
      ; Vdom.Node.span
          ~attrs:[ cls "arb-venue" ]
          [ Vdom.Node.text entry.venue ]
      ; Vdom.Node.span [ Vdom.Node.text entry.title ]
      ; Vdom.Node.a
          ~attrs:
            [ cls "arb-trade-link"
            ; Vdom.Attr.href entry.url
            ; Vdom.Attr.create "target" "_blank"
            ; Vdom.Attr.create "rel" "noopener"
            ]
          [ Vdom.Node.text "trade it ↗" ]
      ; Vdom.Node.span
          ~attrs:[ cls "arb-ask" ]
          [ Vdom.Node.text [%string "ask %{money entry.ask}"] ]
      ]
  in
  let edge_class =
    match Float.( >= ) edge.edge 0. with
    | true -> "arb-edge-positive"
    | false -> "arb-edge-negative"
  in
  let verdict =
    match edge.tradable with
    | true -> [%string "TRADABLE — %{edge.size#Int} contracts deep"]
    | false -> "no trade: cost must stay under $1 after fees"
  in
  let acted = edge.acted || Set.mem acted_keys edge.pair_key in
  let take =
    match edge.tradable with
    | false -> []
    | true ->
      [ Vdom.Node.div
          ~attrs:[ cls "arb-edge-take" ]
          ([ Vdom.Node.span
               ~attrs:[ cls "arb-summary" ]
               [ Vdom.Node.text
                   [%string
                     "booked in the wallet: %{money edge.dollars} if taken \
                      in full"]
               ]
           ; (match acted with
              | true ->
                Vdom.Node.span
                  ~attrs:[ cls "arb-acted-badge" ]
                  [ Vdom.Node.text "✓ traded for real" ]
              | false ->
                button
                  ~class_:"btn-secondary"
                  ~label:"I traded this"
                  (mark_acted edge.pair_key))
           ]
           @
           match acted with
           | true -> []
           | false -> Option.to_list (assist edge))
      ]
  in
  Vdom.Node.div
    ~attrs:
      [ Vdom.Attr.classes
          ([ "arb-edge" ]
           @ if edge.tradable then [ "arb-edge-tradable" ] else [])
      ]
    ([ Vdom.Node.div [ leg "YES" edge.yes; leg "NO" edge.no ]
     ; Vdom.Node.div
         ~attrs:[ cls "arb-edge-math" ]
         [ Vdom.Node.text
             [%string "cost %{money edge.cost} (incl. fees) → edge "]
         ; Vdom.Node.span
             ~attrs:[ cls edge_class ]
             [ Vdom.Node.text (money edge.edge) ]
         ; Vdom.Node.text [%string " · %{verdict}"]
         ]
     ]
     @ take)
;;

(* The "for fun" score: what acting on every booked edge would have paid,
   next to what the user says they really banked. *)
let arb_wallet_view (wallet : Protocol.Wallet.t Or_error.t option) =
  match wallet with
  | None ->
    Vdom.Node.div
      ~attrs:[ cls "arb-wallet" ]
      [ Vdom.Node.div
          ~attrs:[ cls "status" ]
          [ Vdom.Node.text "loading wallet..." ]
      ]
  | Some (Error error) ->
    Vdom.Node.div
      ~attrs:[ cls "arb-wallet" ]
      [ Vdom.Node.div
          ~attrs:[ cls "status" ]
          [ Vdom.Node.text (Error.to_string_hum error) ]
      ]
  | Some (Ok { paper; acted; entries }) ->
    let score label class_ amount =
      Vdom.Node.div
        [ Vdom.Node.div
            ~attrs:[ Vdom.Attr.classes [ "arb-wallet-amount"; class_ ] ]
            [ Vdom.Node.text (sprintf "$%.2f" amount) ]
        ; Vdom.Node.div
            ~attrs:[ cls "arb-wallet-label" ]
            [ Vdom.Node.text label ]
        ]
    in
    let entry (card : Protocol.Wallet_card.t) =
      Vdom.Node.div
        ~attrs:[ cls "arb-wallet-entry" ]
        ([ Vdom.Node.span [ Vdom.Node.text card.summary ] ]
         @ (match card.acted with
            | true ->
              [ Vdom.Node.span
                  ~attrs:[ cls "arb-acted-badge" ]
                  [ Vdom.Node.text "✓ traded" ]
              ]
            | false -> [])
         @ [ Vdom.Node.span
               ~attrs:[ cls "arb-wallet-entry-dollars" ]
               [ Vdom.Node.text (sprintf "$%.2f" card.dollars) ]
           ])
    in
    Vdom.Node.div
      ~attrs:[ cls "arb-wallet" ]
      ([ Vdom.Node.div
           ~attrs:[ cls "arb-wallet-scores" ]
           [ score
               "pretend winnings — every arb found, fees included"
               "arb-wallet-paper"
               paper
           ; score "banked by really trading" "arb-wallet-real" acted
           ]
       ; Vdom.Node.p
           ~attrs:[ cls "arb-wallet-hint" ]
           [ Vdom.Node.text
               "🏦 Every tradable edge a scan finds is booked here at edge × \
                depth, as if you'd taken all of it. Use a card's \"trade \
                it\" links to place the legs for real, then hit \"I traded \
                this\" to move the money from make-believe to banked."
           ]
       ]
       @ List.map entries ~f:entry)
;;

let arb_hedge_dialog_view
  ~(dialog : Hedge_dialog.t)
  ~close
  ~to_step2
  ~set_price
  ~set_count
  ~fire
  =
  let money value = sprintf "$%.2f" value in
  let box children = Vdom.Node.div ~attrs:[ cls "arb-dialog" ] children in
  let warning text =
    Vdom.Node.p ~attrs:[ cls "arb-disclaimer" ] [ Vdom.Node.text text ]
  in
  match dialog with
  | Closed -> Vdom.Node.none
  | Step1 { edge; preflight } ->
    let manual = manual_leg edge in
    let preflight_view =
      match preflight with
      | None ->
        Vdom.Node.div
          ~attrs:[ cls "arb-summary" ]
          [ Vdom.Node.text "checking the rails..." ]
      | Some (Error error) ->
        Vdom.Node.div
          ~attrs:[ cls "arb-summary" ]
          [ Vdom.Node.text
              [%string "pre-flight failed: %{Error.to_string_hum error}"]
          ]
      | Some
          (Ok
            { kill_switch
            ; hedge_title
            ; hedge_is_yes = _
            ; ask_cents
            ; depth
            ; caps_room_dollars
            ; hedgeable
            }) ->
        (match kill_switch with
         | Some reason ->
           Vdom.Node.div
             ~attrs:[ cls "arb-onesided" ]
             [ Vdom.Node.text
                 [%string
                   "kill switch engaged (%{reason}) — the hedge cannot \
                    fire; do not place the manual leg"]
             ]
         | None ->
           Vdom.Node.div
             ~attrs:[ cls "arb-summary" ]
             [ Vdom.Node.text
                 [%string
                   "hedgeable right now: %{hedgeable#Int} contract(s) — \
                    Kalshi ask %{ask_cents#Int}c on \"%{hedge_title}\", \
                    %{depth#Int} deep, %{money caps_room_dollars} of cap \
                    room. Size your manual trade to at most \
                    %{hedgeable#Int}."]
             ])
    in
    let can_continue =
      match preflight with
      | Some (Ok { kill_switch = None; hedgeable; _ }) -> hedgeable > 0
      | Some (Ok { kill_switch = Some (_ : string); _ })
      | Some (Error (_ : Error.t))
      | None ->
        false
    in
    box
      [ Vdom.Node.h2
          ~attrs:[ cls "arb-panel-title" ]
          [ Vdom.Node.text "Assisted hedge — step 1: your leg" ]
      ; Vdom.Node.div
          ~attrs:[ cls "arb-edge-row" ]
          [ Vdom.Node.span
              ~attrs:[ cls "arb-venue" ]
              [ Vdom.Node.text manual.venue ]
          ; Vdom.Node.span [ Vdom.Node.text manual.title ]
          ; Vdom.Node.a
              ~attrs:
                [ cls "arb-trade-link"
                ; Vdom.Attr.href manual.url
                ; Vdom.Attr.create "target" "_blank"
                ; Vdom.Attr.create "rel" "noopener"
                ]
              [ Vdom.Node.text "open the venue ↗" ]
          ; Vdom.Node.span
              ~attrs:[ cls "arb-ask" ]
              [ Vdom.Node.text
                  [%string
                    "target: %{if manual_is_yes edge then \"YES\" else \
                     \"NO\"} @ %{money manual.ask}"]
              ]
          ]
      ; preflight_view
      ; warning
          "The app cannot place or verify this leg — only place it if you \
           and your account may legally trade on that venue. Everything you \
           enter in step 2 is on your say-so and will be marked \
           self-reported."
      ; Vdom.Node.div
          ~attrs:[ cls "arb-pair-actions" ]
          [ button
              ~enabled:can_continue
              ~class_:"btn-primary"
              ~label:"I placed it — continue"
              to_step2
          ; button ~class_:"btn-secondary" ~label:"Cancel" close
          ]
      ]
  | Step2 { edge = _; preflight; nonce = _; price_text; count_text; firing }
    ->
    let max_cents = preflight.ask_cents + hedge_slippage_cents in
    box
      [ Vdom.Node.h2
          ~attrs:[ cls "arb-panel-title" ]
          [ Vdom.Node.text "Assisted hedge — step 2: confirm your fill" ]
      ; Vdom.Node.div
          ~attrs:[ cls "arb-controls" ]
          [ Vdom.Node.label
              ~attrs:[ cls "control-label" ]
              [ Vdom.Node.text "your fill price (cents)" ]
          ; Vdom.Node.input
              ~attrs:
                [ cls "num-input"
                ; Vdom.Attr.value price_text
                ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
                    set_price text)
                ]
              ()
          ; Vdom.Node.label
              ~attrs:[ cls "control-label" ]
              [ Vdom.Node.text "contracts filled" ]
          ; Vdom.Node.input
              ~attrs:
                [ cls "num-input"
                ; Vdom.Attr.value count_text
                ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
                    set_count text)
                ]
              ()
          ]
      ; Vdom.Node.p
          ~attrs:[ cls "arb-panel-hint" ]
          [ Vdom.Node.text
              [%string
                "The app will buy the opposite side on Kalshi \
                 (\"%{preflight.hedge_title}\"), sized to your confirmed \
                 count, paying at most %{max_cents#Int}c per contract — it \
                 refuses rather than pays more."]
          ]
      ; Vdom.Node.div
          ~attrs:[ cls "arb-pair-actions" ]
          [ button
              ~enabled:(not firing)
              ~class_:"btn-primary"
              ~label:
                (if firing
                 then "firing hedge..."
                 else [%string "Fire Kalshi hedge (max %{max_cents#Int}c)"])
              fire
          ; button
              ~enabled:(not firing)
              ~class_:"btn-secondary"
              ~label:"I didn't trade — cancel"
              close
          ]
      ]
  | Finished { edge = _; result } ->
    let body =
      match result with
      | Hedged { price; count; fee; unhedged } ->
        let base =
          Vdom.Node.div
            ~attrs:[ cls "arb-summary" ]
            [ Vdom.Node.text
                [%string
                  "hedged: %{count#Int} contract(s) @ %{money price}, fee \
                   %{money fee} — banked into the wallet as self-reported"]
            ]
        in
        (match unhedged > 0 with
         | false -> [ base ]
         | true ->
           [ base
           ; Vdom.Node.div
               ~attrs:[ cls "arb-onesided" ]
               [ Vdom.Node.text
                   [%string
                     "⚠ PARTIAL: you are still one-sided by %{unhedged#Int} \
                      contract(s) — the book ran out of depth. Manage the \
                      remainder by hand."]
               ]
           ])
      | Refused reason ->
        [ Vdom.Node.div
            ~attrs:[ cls "arb-onesided" ]
            [ Vdom.Node.text
                [%string
                  "hedge refused (nothing was sent): %{reason}. You are \
                   one-sided by your manual leg — fix the condition and \
                   retry from the edge card."]
            ]
        ]
      | Failed { error; detail; unhedged } ->
        [ Vdom.Node.div
            ~attrs:[ cls "arb-onesided" ]
            [ Vdom.Node.text
                [%string
                  "⚠ HEDGE FAILED (%{Sexp.to_string [%sexp (error : \
                   Protocol.Venue_error.t)]}): you are ONE-SIDED by \
                   %{unhedged#Int} contract(s). Detail: %{detail}"]
            ]
        ]
    in
    box
      ((Vdom.Node.h2
          ~attrs:[ cls "arb-panel-title" ]
          [ Vdom.Node.text "Assisted hedge — result" ]
        :: body)
       @ [ Vdom.Node.div
             ~attrs:[ cls "arb-pair-actions" ]
             [ button ~class_:"btn-secondary" ~label:"Close" close ]
         ])
;;

let arb_detect_panel ~scan_state ~scan ~wallet ~mark_acted ~assist ~dialog =
  let running =
    match (scan_state : Scan_state.t) with
    | Running -> true
    | Idle | Done (_ : Protocol.Scan_report.t) -> false
  in
  (* The wallet is the freshest word on what's been acted: cards from an
     older scan pick their badges up from it. *)
  let acted_keys =
    match (wallet : Protocol.Wallet.t Or_error.t option) with
    | None | Some (Error (_ : Error.t)) -> String.Set.empty
    | Some (Ok { entries; _ }) ->
      String.Set.of_list
        (List.filter_map entries ~f:(fun (card : Protocol.Wallet_card.t) ->
           match card.acted with true -> Some card.pair_key | false -> None))
  in
  let body =
    match (scan_state : Scan_state.t) with
    | Idle -> Vdom.Node.div []
    | Running ->
      Vdom.Node.div
        ~attrs:[ cls "arb-summary" ]
        [ Vdom.Node.text "fetching live order books on both venues..." ]
    | Done { pairs; legs_priced; edges; tradable } ->
      Vdom.Node.div
        ([ Vdom.Node.div
             ~attrs:[ cls "arb-summary" ]
             [ Vdom.Node.text
                 [%string
                   "%{pairs#Int} approved pair(s), %{legs_priced#Int} legs \
                    priced from live books, %{tradable#Int} tradable"]
             ]
         ]
         @ List.map edges ~f:(arb_edge_view ~mark_acted ~acted_keys ~assist)
        )
  in
  Vdom.Node.div
    ~attrs:[ cls "arb-panel" ]
    [ Vdom.Node.h2
        ~attrs:[ cls "arb-panel-title" ]
        [ Vdom.Node.text "3 · Arb Pairs" ]
    ; Vdom.Node.p
        ~attrs:[ cls "arb-panel-hint" ]
        [ Vdom.Node.text
            "One tick of the paper bot: fetch each approved pair's live \
             books and price YES on one venue against NO on the other, fees \
             included. An edge means buying both sides pays $1 for less \
             than $1. Each card's two rows are the same event — the venues \
             just word the title differently."
        ]
    ; arb_wallet_view wallet
    ; dialog
    ; button
        ~enabled:(not running)
        ~class_:"btn-primary"
        ~label:"Scan approved pairs"
        scan
    ; body
    ]
;;

let arbitrage_page (local_ graph) =
  let section, set_section = Bonsai.state Arb_section.Sweep graph in
  let tab, set_tab = Bonsai.state Protocol.Pair_status.Proposed graph in
  let pairs, set_pairs = Bonsai.state_opt graph in
  let wallet, set_wallet = Bonsai.state_opt graph in
  let hedge_capability, set_hedge_capability = Bonsai.state_opt graph in
  let hedge_dialog, set_hedge_dialog =
    Bonsai.state Hedge_dialog.Closed graph
  in
  let sweep_state, set_sweep_state = Bonsai.state Sweep_state.Idle graph in
  let scan_state, set_scan_state = Bonsai.state Scan_state.Idle graph in
  let llm_state, set_llm_state = Bonsai.state Llm_state.Idle graph in
  let auto_state, set_auto_state = Bonsai.state Auto_state.Idle graph in
  let threshold, set_threshold = Bonsai.state "0.35" graph in
  let auto_threshold, set_auto_threshold = Bonsai.state "0.50" graph in
  let api_key, set_api_key = Bonsai.state "" graph in
  let dispatch_pairs = Rpc_effect.Rpc.dispatcher Protocol.get_pairs graph in
  let dispatch_decide =
    Rpc_effect.Rpc.dispatcher Protocol.decide_pair graph
  in
  let dispatch_llm =
    Rpc_effect.Rpc.dispatcher Protocol.llm_review_pairs graph
  in
  let dispatch_auto =
    Rpc_effect.Rpc.dispatcher Protocol.auto_review_pairs graph
  in
  let dispatch_sweep = Rpc_effect.Rpc.dispatcher Protocol.run_sweep graph in
  let dispatch_scan = Rpc_effect.Rpc.dispatcher Protocol.scan_edges graph in
  let dispatch_wallet =
    Rpc_effect.Rpc.dispatcher Protocol.get_wallet graph
  in
  let dispatch_mark = Rpc_effect.Rpc.dispatcher Protocol.mark_acted graph in
  let dispatch_capability =
    Rpc_effect.Rpc.dispatcher Protocol.get_execution_capability graph
  in
  let dispatch_preflight =
    Rpc_effect.Rpc.dispatcher Protocol.preflight_hedge graph
  in
  let dispatch_hedge =
    Rpc_effect.Rpc.dispatcher Protocol.execute_hedge graph
  in
  let on_activate =
    let%map dispatch_pairs and set_pairs in
    let%bind.Effect response =
      dispatch_pairs Protocol.Pair_status.Proposed
    in
    set_pairs (Some (Or_error.join response))
  in
  Bonsai.Edge.lifecycle ~on_activate graph;
  let%arr section
  and set_section
  and tab
  and set_tab
  and pairs
  and set_pairs
  and wallet
  and set_wallet
  and hedge_capability
  and set_hedge_capability
  and hedge_dialog
  and set_hedge_dialog
  and sweep_state
  and set_sweep_state
  and scan_state
  and set_scan_state
  and llm_state
  and set_llm_state
  and auto_state
  and set_auto_state
  and threshold
  and set_threshold
  and auto_threshold
  and set_auto_threshold
  and api_key
  and set_api_key
  and dispatch_pairs
  and dispatch_decide
  and dispatch_llm
  and dispatch_auto
  and dispatch_sweep
  and dispatch_scan
  and dispatch_wallet
  and dispatch_mark
  and dispatch_capability
  and dispatch_preflight
  and dispatch_hedge in
  let load status =
    let open Effect.Let_syntax in
    let%bind response = dispatch_pairs status in
    set_pairs (Some (Or_error.join response))
  in
  let select_tab status =
    Effect.Many [ set_tab status; set_pairs None; load status ]
  in
  let load_wallet =
    let open Effect.Let_syntax in
    let%bind response = dispatch_wallet () in
    set_wallet (Some (Or_error.join response))
  in
  let select_section new_section =
    (* Re-entering a section refreshes what it shows — Review's listing and
       the Pairs wallet both change behind this page's back. *)
    Effect.Many
      [ set_section new_section
      ; (match (new_section : Arb_section.t) with
         | Review -> Effect.Many [ set_pairs None; load tab ]
         | Pairs ->
           Effect.Many
             [ load_wallet
             ; (let open Effect.Let_syntax in
                let%bind response = dispatch_capability () in
                set_hedge_capability (Some (Or_error.join response)))
             ]
         | Sweep -> Effect.Ignore)
      ]
  in
  let mark_acted pair_key =
    let open Effect.Let_syntax in
    let%bind response = dispatch_mark pair_key in
    set_wallet (Some (Or_error.join response))
  in
  let close_hedge = set_hedge_dialog Hedge_dialog.Closed in
  let open_hedge (edge : Protocol.Edge_card.t) =
    let open Effect.Let_syntax in
    let%bind () =
      set_hedge_dialog (Hedge_dialog.Step1 { edge; preflight = None })
    in
    let%bind response =
      dispatch_preflight
        { Protocol.Preflight_request.pair_key = edge.pair_key
        ; manual_is_yes = manual_is_yes edge
        }
    in
    set_hedge_dialog
      (Hedge_dialog.Step1 { edge; preflight = Some (Or_error.join response) })
  in
  (* Minted at render; captured by the click that moves to step 2 and reused
     for every retry of that confirmation. *)
  let fresh_nonce =
    (* [Int63], not [int]: under js_of_ocaml the native int is 32 bits and
       [to_int_ns_since_epoch] raises. *)
    let ms =
      Int63.( / )
        (Time_ns.to_int63_ns_since_epoch (Time_ns.now ()))
        (Int63.of_int 1_000_000)
    in
    [%string "%{ms#Int63}"]
  in
  let hedge_to_step2 =
    match hedge_dialog with
    | Step1 { edge; preflight = Some (Ok preflight) } ->
      let manual = manual_leg edge in
      set_hedge_dialog
        (Hedge_dialog.Step2
           { edge
           ; preflight
           ; nonce = fresh_nonce
           ; price_text =
               Int.to_string
                 (Int.of_float (Float.round_nearest (manual.ask *. 100.)))
           ; count_text =
               Int.to_string (Int.min edge.size preflight.hedgeable)
           ; firing = false
           })
    | Closed
    | Step1 { preflight = Some (Error (_ : Error.t)) | None; _ }
    | Step2 _ | Finished _ ->
      Effect.Ignore
  in
  let hedge_set_price text =
    match hedge_dialog with
    | Step2 step2 ->
      set_hedge_dialog (Hedge_dialog.Step2 { step2 with price_text = text })
    | Closed | Step1 _ | Finished _ -> Effect.Ignore
  in
  let hedge_set_count text =
    match hedge_dialog with
    | Step2 step2 ->
      set_hedge_dialog (Hedge_dialog.Step2 { step2 with count_text = text })
    | Closed | Step1 _ | Finished _ -> Effect.Ignore
  in
  let hedge_fire =
    match hedge_dialog with
    | Step2
        ({ edge; preflight; nonce; price_text; count_text; firing = _ } as
         step2) ->
      (match Int.of_string_opt price_text, Int.of_string_opt count_text with
       | Some price, Some count when price > 0 && count > 0 ->
         let open Effect.Let_syntax in
         let%bind () =
           set_hedge_dialog (Hedge_dialog.Step2 { step2 with firing = true })
         in
         let%bind response =
           dispatch_hedge
             { Protocol.Hedge_request.pair_key = edge.pair_key
             ; nonce
             ; manual_venue = (manual_leg edge).venue
             ; manual_is_yes = manual_is_yes edge
             ; manual_price_cents = price
             ; manual_count = count
             ; max_hedge_price_cents =
                 preflight.ask_cents + hedge_slippage_cents
             }
         in
         (match Or_error.join response with
          | Error error ->
            set_hedge_dialog
              (Hedge_dialog.Finished
                 { edge
                 ; result =
                     Protocol.Hedge_result.Refused
                       (Error.to_string_hum error)
                 })
          | Ok result ->
            Effect.Many
              [ set_hedge_dialog (Hedge_dialog.Finished { edge; result })
              ; load_wallet
              ])
       | None, _ | _, None | Some _, Some _ -> Effect.Ignore)
    | Closed | Step1 _ | Finished _ -> Effect.Ignore
  in
  let decide ~index ~new_status =
    let open Effect.Let_syntax in
    let%bind (_ : Protocol.Pair_card.t Or_error.t Or_error.t) =
      dispatch_decide { Protocol.Decide_request.tab; index; new_status }
    in
    load tab
  in
  let run_llm =
    let open Effect.Let_syntax in
    let api_key =
      match String.strip api_key with "" -> None | key -> Some key
    in
    let%bind () = set_llm_state Running in
    let%bind response =
      dispatch_llm { Protocol.Llm_review_request.api_key }
    in
    match Or_error.join response with
    | Error error -> set_llm_state (Failed (Error.to_string_hum error))
    | Ok summary ->
      Effect.Many [ set_llm_state (Done summary); set_pairs None; load tab ]
  in
  let run_auto =
    match Float.of_string_opt auto_threshold with
    | None -> Effect.Ignore
    | Some threshold ->
      let open Effect.Let_syntax in
      let%bind () = set_auto_state Running in
      let%bind response =
        dispatch_auto { Protocol.Auto_review_request.threshold }
      in
      (match Or_error.join response with
       | Error (_ : Error.t) -> set_auto_state Idle
       | Ok summary ->
         Effect.Many
           [ set_auto_state (Done summary); set_pairs None; load tab ])
  in
  let run_sweep =
    match Float.of_string_opt threshold with
    | None -> Effect.Ignore
    | Some threshold ->
      let open Effect.Let_syntax in
      let%bind () = set_sweep_state Running in
      let%bind response =
        dispatch_sweep { Protocol.Sweep_request.threshold }
      in
      (match Or_error.join response with
       | Error (_ : Error.t) -> set_sweep_state Idle
       | Ok summary ->
         Effect.Many [ set_sweep_state (Done summary); load tab ])
  in
  let scan =
    let open Effect.Let_syntax in
    let%bind () = set_scan_state Running in
    let%bind response = dispatch_scan () in
    match Or_error.join response with
    | Error (_ : Error.t) -> set_scan_state Idle
    | Ok report ->
      (* The scan just booked its tradable edges — refresh the score. *)
      Effect.Many [ set_scan_state (Done report); load_wallet ]
  in
  let panel =
    match section with
    | Arb_section.Sweep ->
      arb_sweep_panel ~sweep_state ~threshold ~set_threshold ~run:run_sweep
    | Review ->
      let llm =
        arb_llm_panel ~llm_state ~api_key ~set_api_key ~run:run_llm
      in
      let auto =
        arb_auto_panel
          ~auto_state
          ~auto_threshold
          ~set_auto_threshold
          ~run:run_auto
      in
      arb_review_panel ~tab ~pairs ~select_tab ~decide ~llm ~auto
    | Pairs ->
      let live =
        match hedge_capability with
        | Some (Ok { Protocol.Execution_capability.live; reason = _ }) ->
          live
        | Some (Error (_ : Error.t)) | None -> false
      in
      let assist (edge : Protocol.Edge_card.t) =
        match live && edge.tradable with
        | false -> None
        | true ->
          Some
            (button
               ~class_:"btn-primary"
               ~label:"Hedge assisted…"
               (open_hedge edge))
      in
      let dialog =
        arb_hedge_dialog_view
          ~dialog:hedge_dialog
          ~close:close_hedge
          ~to_step2:hedge_to_step2
          ~set_price:hedge_set_price
          ~set_count:hedge_set_count
          ~fire:hedge_fire
      in
      arb_detect_panel ~scan_state ~scan ~wallet ~mark_acted ~assist ~dialog
  in
  Vdom.Node.div
    [ arb_stage_banner ~current:section ~select:select_section; panel ]
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
  let arbitrage = arbitrage_page graph in
  let%arr page and set_page and markets_result and bots and arbitrage in
  let body =
    match page with
    | Markets -> markets_page_view markets_result
    | Bots -> bots
    | Arbitrage -> arbitrage
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
