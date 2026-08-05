(* The web app's client half: a Bonsai single-page app served by
   [app/server]. The pages live beside this file — {!Markets_page},
   {!Bots_page}, {!Arb_page}, {!Wallet_page} — over the shared view
   helpers in {!Ui} and the chart renderer in {!Chart_view}. This
   file holds the page enum and the shell that swaps pages; the
   stylesheet is a static asset ([app/client/static]) served by the
   server alongside the bundle. *)

open! Core
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
  .card-clickable { cursor: pointer; }
  .modal-backdrop {
    position: fixed;
    inset: 0;
    background: rgba(5, 8, 14, 0.65);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 10;
  }
  .modal {
    background: #141926;
    border: 1px solid #2a3040;
    border-radius: 12px;
    padding: 18px 22px;
    width: 720px;
    max-width: 90vw;
    max-height: 85vh;
    overflow-y: auto;
  }
  .modal-header {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    gap: 12px;
  }
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
  .search-input, .num-input, .interval-select, .saved-bot-select,
  .bot-name-input {
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
  .interval-select, .saved-bot-select { width: auto; }
  .bot-name-input { width: 180px; }
  .saved-bot-row {
    display: flex; gap: 10px; align-items: center; margin: 12px 0;
  }
  .save-confirm { color: #4ade80; font-size: 13px; align-self: center; }
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
    border-radius: 12px;
    padding: 5px 12px;
    font-size: 13px;
  }
  .chip-lines { display: flex; flex-direction: column; gap: 2px; }
  .chip-ticker {
    color: #8b93a7; font-size: 11px; font-family: monospace;
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
  .rule-edit {
    background: none;
    border: none;
    color: #8b93a7;
    cursor: pointer;
    font-size: 14px;
    padding: 0;
  }
  .rule-edit:hover { color: #7dd3fc; }
  .entry-buttons { display: flex; gap: 8px; flex: none; }
  .stage-header { display: flex; justify-content: space-between; align-items: flex-start; gap: 20px; }
  .rules-layout { display: flex; gap: 26px; align-items: flex-start; margin: 12px 0 18px; width: 100%; }
  .config-col { display: flex; flex-direction: column; gap: 12px; width: 180px; flex: none; }
  .define-col { display: flex; flex-direction: column; gap: 8px; flex: 1 1 0; min-width: 300px; }
  .define-input {
    background: #10151f;
    border: 1px solid #2a3040;
    border-radius: 8px;
    color: #e8eaf0;
    font-family: monospace;
    font-size: 14px;
    padding: 10px 14px;
    width: 100%;
    box-sizing: border-box;
    white-space: pre;
    overflow-x: auto;
    overflow-y: hidden;
    resize: none;
    scrollbar-width: thin;
    scrollbar-color: rgba(139, 147, 167, 0.35) transparent;
  }
  .define-input::-webkit-scrollbar { height: 8px; background: transparent; }
  .define-input::-webkit-scrollbar-track { background: transparent; }
  .define-input::-webkit-scrollbar-thumb {
    background: rgba(139, 147, 167, 0.35);
    border-radius: 999px;
  }
  .define-input:focus { outline: none; border-color: #7dd3fc; }
  .define-col .suggestions { width: 100%; box-sizing: border-box; margin-top: 0; }
  .rules-col, .vars-col {
    display: flex;
    flex-direction: column;
    gap: 10px;
    flex: 1 1 0;
    min-width: 260px;
    background: #10151f;
    border: 1px solid #232a3b;
    border-radius: 12px;
    padding: 14px 16px;
    box-sizing: border-box;
  }
  .rule-entry span, .var-def-subtext { overflow-wrap: anywhere; }
  .list-heading {
    color: #8b93a7;
    font-size: 12px;
    text-transform: uppercase;
    letter-spacing: 0.08em;
  }
  .rule-entry {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 10px;
    font-family: monospace;
    font-size: 13px;
  }
  .var-entry { font-size: 14px; }
  .var-entry-head {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 10px;
  }
  .var-def-subtext {
    color: #8b93a7;
    font-size: 11px;
    font-family: monospace;
    margin-top: 2px;
  }
  .checkbox { width: 18px; height: 18px; accent-color: #0ea5e9; margin-top: 8px; }
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
  .fill-reason { color: #8b93a7; font-size: 12px; max-width: 480px; }
  .stats-grid { display: flex; flex-wrap: wrap; gap: 14px; margin: 10px 0 22px; }
  .stat-tile {
    background: #141926;
    border: 1px solid #232a3b;
    border-radius: 12px;
    padding: 12px 18px;
    min-width: 120px;
  }
  .stat-label { color: #8b93a7; font-size: 12px; margin-bottom: 6px; }
  .stat-value { font-size: 20px; font-weight: 600; }
  .stat-pos { color: #4ade80; }
  .stat-neg { color: #f87171; }
  .stat-inventory { font-family: monospace; font-size: 13px; line-height: 1.6; }
  .charts-grid {
    display: grid;
    grid-template-columns: repeat(2, max-content);
    gap: 20px;
    margin: 0 0 24px;
  }
  .charts-grid .chart-box { margin: 0; }
  .chip-row { display: flex; align-items: center; gap: 12px; }
  .chip-row-label { color: #8b93a7; font-size: 12px; min-width: 58px; }
  .fills-toggle { margin: 0 0 24px; }
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
  .manual-body { font-size: 13px; color: #a5b4d0; }
  .manual-body p { margin: 6px 0; }
  .manual-section-title {
    font-size: 12px; text-transform: uppercase; letter-spacing: 0.08em;
    color: #7dd3fc; margin: 18px 0 4px;
  }
  .manual-code {
    background: #0d1220; border: 1px solid #232a3b; border-radius: 8px;
    color: #e8eaf0; font-family: ui-monospace, monospace; font-size: 13px;
    padding: 8px 12px; margin: 6px 0; overflow-x: auto;
  }
  .manual-note { color: #fbbf24; margin: 6px 0; }
|}
;;
open Ui

module Page = struct
  type t =
    | Markets
    | Bots
    | Arbitrage
    | Wallet
  [@@deriving sexp_of, compare, equal, enumerate]

  let name = function
    | Markets -> "Markets"
    | Bots -> "Bots"
    | Arbitrage -> "Arbitrage"
    | Wallet -> "Wallet"
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

let volume_string (card : Card.t) =
  match card.volume with
  | None -> "volume unreported"
  | Some (Contracts contracts) ->
    let count = Int.to_string_hum ~delimiter:',' (Size.to_int contracts) in
    [%string "%{count} contracts"]
  | Some (Notional dollars) ->
    [%string "%{Price.to_string_dollar dollars} traded"]
;;

(* Keeps a market-chip name to roughly one row of the rules layout. *)
let max_title_length = 60

let truncate_title title =
  match String.length title > max_title_length with
  | false -> title
  | true -> String.prefix title max_title_length ^ "..."
;;

let card_view ~on_select (card : Card.t) =
  (* Only history-capable markets can open the detail popup; the others stay
     inert (no pointer cursor either). *)
  let attrs =
    match card.has_price_history with
    | false -> [ cls "card" ]
    | true ->
      [ Vdom.Attr.classes [ "card"; "card-clickable" ]
      ; on_click (on_select card)
      ]
  in
  Vdom.Node.div
    ~attrs
    [ Vdom.Node.div ~attrs:[ cls "card-title" ] [ Vdom.Node.text card.title ]
    ; Vdom.Node.div
        ~attrs:[ cls "card-slug" ]
        [ Vdom.Node.text (Slug.to_string card.slug) ]
    ; Vdom.Node.div
        ~attrs:[ cls "volume-badge" ]
        [ Vdom.Node.text (volume_string card) ]
    ]
;;

let category_section ~on_select (category, cards) =
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
    ; Vdom.Node.div
        ~attrs:[ cls "card-row" ]
        (List.map cards ~f:(card_view ~on_select))
    ]
;;

let markets_view cards ~on_select =
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
  | groups ->
    Vdom.Node.div (List.map groups ~f:(category_section ~on_select))
;;

let markets_page_view markets ~on_select =
  match markets with
  | None ->
    Vdom.Node.div
      ~attrs:[ cls "status" ]
      [ Vdom.Node.text "loading markets..." ]
  | Some (Error error) ->
    Vdom.Node.pre [ Vdom.Node.text (Error.to_string_hum error) ]
  | Some (Ok cards) -> markets_view cards ~on_select
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

(* What the backtest runs alongside the bot. A [Saved_bot] holds only the
   name; the record is looked up in the saved list at run time, so a bot
   deleted after being picked simply invalidates the Run button. *)
module Compare_choice = struct
  type t =
    | No_comparison
    | Dumb_bots
    | Saved_bot of string
  [@@deriving sexp_of, equal]
end

(* Saved bots persist per-browser in localStorage as one sexp blob; a missing
   or unparseable entry is just "no saved bots" (see
   {!Client_logic.Saved_bot}). *)
let saved_bots_storage_key = Js_of_ocaml.Js.string "arbiter-saved-bots"

let load_saved_bots () =
  match
    Js_of_ocaml.Js.Optdef.to_option
      Js_of_ocaml.Dom_html.window##.localStorage
  with
  | None -> []
  | Some storage ->
    (match
       Js_of_ocaml.Js.Opt.to_option (storage##getItem saved_bots_storage_key)
     with
     | None -> []
     | Some text ->
       Client_logic.Saved_bot.list_of_string (Js_of_ocaml.Js.to_string text))
;;

let store_saved_bots bots =
  match
    Js_of_ocaml.Js.Optdef.to_option
      Js_of_ocaml.Dom_html.window##.localStorage
  with
  | None -> ()
  | Some storage ->
    storage##setItem
      saved_bots_storage_key
      (Js_of_ocaml.Js.string (Client_logic.Saved_bot.list_to_string bots))
;;

let store_saved_bots_effect = Effect.of_sync_fun store_saved_bots

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

(* Without [on_remove] the chips are a plain listing — no dead ✕ buttons. *)
let chips ?on_remove selected =
  Vdom.Node.div
    ~attrs:[ cls "chips" ]
    (List.map selected ~f:(fun (card : Card.t) ->
       let remove =
         match on_remove with
         | None -> []
         | Some on_remove ->
           [ Vdom.Node.button
               ~attrs:[ cls "chip-remove"; on_click (on_remove card) ]
               [ Vdom.Node.text "✕" ]
           ]
       in
       let name_and_ticker =
         Vdom.Node.div
           ~attrs:[ cls "chip-lines" ]
           [ Vdom.Node.div
               [ Vdom.Node.text
                   [%string "name: %{truncate_title card.title}"]
               ]
           ; Vdom.Node.div
               ~attrs:[ cls "chip-ticker" ]
               [ Vdom.Node.text
                   [%string "ticker: %{Slug.to_string card.slug}"]
               ]
           ]
       in
       Vdom.Node.div ~attrs:[ cls "chip" ] (name_and_ticker :: remove)))
;;

(* Column 2: one input takes either statement kind — a [name = expr] line
   becomes a variable, anything else a rule — one per Enter press. Nothing
   compiles until Run; the server reports parse errors then. *)
let define_column ~draft ~set_draft ~suggestions ~on_pick ~submit =
  Vdom.Node.div
    ~attrs:[ cls "define-col" ]
    [ Vdom.Node.label
        ~attrs:[ cls "control-label" ]
        [ Vdom.Node.text "type a rule or a variable; enter adds it" ]
    ; (* A one-row textarea rather than an input: with wrapping off, long
         statements overflow into a horizontal scrollbar instead of hiding.
         Enter must be swallowed (Prevent_default) or it inserts a newline
         before submit clears the draft. *)
      Vdom.Node.textarea
        ~attrs:
          [ cls "define-input"
          ; Vdom.Attr.placeholder "every 2h buy 1 <ticker> yes"
          ; Vdom.Attr.create "rows" "1"
          ; Vdom.Attr.create "wrap" "off"
          ; Vdom.Attr.value draft
          ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
              set_draft text)
          ; Vdom.Attr.on_keydown (fun event ->
              match Js_of_ocaml.Dom_html.Keyboard_code.of_event event with
              | Enter ->
                event##preventDefault;
                submit
              | Tab ->
                (* With nothing to complete, leave the default alone so Tab
                   still moves keyboard focus out of the editor. *)
                (match suggestions with
                 | [] -> Effect.Ignore
                 | first :: _ ->
                   event##preventDefault;
                   on_pick first)
              | _ -> Effect.Ignore)
          ]
        []
    ; button ~class_:"btn-secondary" ~label:"Add" submit
    ; suggestion_list suggestions ~on_pick
    ]
;;

(* Column 3: the entered rules, in order, each removable or editable. Edit
   moves the rule back into the definition box (re-adding on Enter as usual),
   so a re-added rule lands at the end of the list. *)
let rules_column ~rules ~set_rules ~set_draft =
  let entry rule =
    let without_rule =
      set_rules
        (List.filter rules ~f:(fun kept -> not (String.equal kept rule)))
    in
    Vdom.Node.div
      ~attrs:[ cls "rule-entry" ]
      [ Vdom.Node.span [ Vdom.Node.text rule ]
      ; Vdom.Node.div
          ~attrs:[ cls "entry-buttons" ]
          [ Vdom.Node.button
              ~attrs:
                [ cls "rule-edit"
                ; on_click (Effect.Many [ set_draft rule; without_rule ])
                ]
              [ Vdom.Node.text "✎" ]
          ; Vdom.Node.button
              ~attrs:[ cls "chip-remove"; on_click without_rule ]
              [ Vdom.Node.text "✕" ]
          ]
      ]
  in
  let body =
    match rules with
    | [] ->
      [ Vdom.Node.p ~attrs:[ cls "status" ] [ Vdom.Node.text "no rules yet" ]
      ]
    | rules -> List.map rules ~f:entry
  in
  Vdom.Node.div
    ~attrs:[ cls "rules-col" ]
    (Vdom.Node.div ~attrs:[ cls "list-heading" ] [ Vdom.Node.text "rules" ]
     :: body)
;;

(* Column 4: builtins first (fixed), then the chosen markets in their program
   spelling (raw venue slug as subtext), then user definitions — [$name] in
   plain ink with the defining expression as gray subtext. *)
let variables_column ~tickers ~variables ~set_variables =
  let builtin name =
    Vdom.Node.div ~attrs:[ cls "var-entry" ] [ Vdom.Node.text name ]
  in
  let market slug =
    let name = Parser.Ticker_name.normalize (Slug.to_string slug) in
    Vdom.Node.div
      ~attrs:[ cls "var-entry" ]
      [ Vdom.Node.text name
      ; Vdom.Node.div
          ~attrs:[ cls "var-def-subtext" ]
          [ Vdom.Node.text (Slug.to_string slug) ]
      ]
  in
  let defined definition =
    match Client_logic.Rule_candidates.definition_name definition with
    | None -> Vdom.Node.none
    | Some name ->
      let subtext =
        match String.lsplit2 definition ~on:'=' with
        | None -> definition
        | Some ((_ : string), rhs) -> [%string "= %{String.strip rhs}"]
      in
      Vdom.Node.div
        ~attrs:[ cls "var-entry" ]
        [ Vdom.Node.div
            ~attrs:[ cls "var-entry-head" ]
            [ Vdom.Node.text name
            ; Vdom.Node.button
                ~attrs:
                  [ cls "chip-remove"
                  ; on_click
                      (set_variables
                         (List.filter variables ~f:(fun kept ->
                            not (String.equal kept definition))))
                  ]
                [ Vdom.Node.text "✕" ]
            ]
        ; Vdom.Node.div
            ~attrs:[ cls "var-def-subtext" ]
            [ Vdom.Node.text subtext ]
        ]
  in
  Vdom.Node.div
    ~attrs:[ cls "vars-col" ]
    (Vdom.Node.div
       ~attrs:[ cls "list-heading" ]
       [ Vdom.Node.text "variables" ]
     :: (List.map Client_logic.Rule_candidates.builtins ~f:builtin
         @ List.map tickers ~f:market
         @ List.map variables ~f:defined))
;;

let pick_view
  ~all_cards
  ~selected
  ~set_selected
  ~search_text
  ~set_search_text
  ~saved_bots
  ~picked_bot
  ~set_picked_bot
  ~load_bot
  ~delete_bot
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
  let saved_bot_row =
    match saved_bots with
    | [] -> Vdom.Node.none
    | _ :: _ ->
      let placeholder_option =
        Vdom.Node.option
          ~attrs:
            ([ Vdom.Attr.value "" ]
             @
             match String.is_empty picked_bot with
             | true -> [ Vdom.Attr.selected ]
             | false -> [])
          [ Vdom.Node.text "saved bots..." ]
      in
      let bot_option (bot : Client_logic.Saved_bot.t) =
        Vdom.Node.option
          ~attrs:
            ([ Vdom.Attr.value bot.name ]
             @
             match String.equal bot.name picked_bot with
             | true -> [ Vdom.Attr.selected ]
             | false -> [])
          [ Vdom.Node.text bot.name ]
      in
      let picked = Client_logic.Saved_bot.find saved_bots ~name:picked_bot in
      let with_picked action =
        match picked with None -> Effect.Ignore | Some bot -> action bot
      in
      Vdom.Node.div
        ~attrs:[ cls "saved-bot-row" ]
        [ Vdom.Node.select
            ~attrs:
              [ cls "saved-bot-select"
              ; Vdom.Attr.on_change (fun (_ : _ Js_of_ocaml.Js.t) name ->
                  set_picked_bot name)
              ]
            (placeholder_option :: List.map saved_bots ~f:bot_option)
        ; button
            ~enabled:(Option.is_some picked)
            ~class_:"btn-secondary"
            ~label:"Load"
            (with_picked load_bot)
        ; button
            ~enabled:(Option.is_some picked)
            ~class_:"btn-secondary"
            ~label:"Delete"
            (with_picked (fun (bot : Client_logic.Saved_bot.t) ->
               delete_bot bot.name))
        ]
  in
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
    ; saved_bot_row
    ; button
        ~enabled:(count >= 1 && count <= max_markets)
        ~class_:"btn-primary"
        ~label:"Continue to rules"
        continue
    ]
;;

(* Every example below is real syntax, checked against the grammar in
   [lib/language_parser] (see parse.mli's doc block). If the language
   changes, this text must change with it. *)
let manual_view ~close =
  let paragraph text = Vdom.Node.p [ Vdom.Node.text text ] in
  let section_title text =
    Vdom.Node.h3
      ~attrs:[ cls "manual-section-title" ]
      [ Vdom.Node.text text ]
  in
  let code_block lines =
    Vdom.Node.create
      "pre"
      ~attrs:[ cls "manual-code" ]
      [ Vdom.Node.text (String.concat lines ~sep:"\n") ]
  in
  let note text =
    Vdom.Node.p ~attrs:[ cls "manual-note" ] [ Vdom.Node.text text ]
  in
  let body =
    Vdom.Node.div
      ~attrs:[ cls "manual-body" ]
      [ paragraph
          "A bot is a list of statements, one per line. Each line is either \
           a rule (something to do) or a variable definition. Keywords work \
           in any case. The examples use my_market as the ticker; swap in \
           one of yours from the variables panel, spelled the way it \
           appears there."
      ; section_title "conditions"
      ; code_block [ "$cash > 50 && my_market < 0.40" ]
      ; paragraph
          "Conditions work like in most languages: compare two amounts with \
           > >= < <= == or !=, and combine comparisons with ! (not), && \
           (and), ^ (exclusive or), || (or), and parentheses. true and \
           false also work. A bare ticker is that market's current yes \
           price in dollars, so my_market < 0.40 reads as \"yes costs under \
           40 cents\". One condition is unique to this language: my_market \
           down by 10% since 1d ago is true when the yes price fell that \
           much over the window (use up for rises, by 0.05 for an absolute \
           five-cent move)."
      ; section_title "actions: buy and sell"
      ; code_block
          [ "buy 2 my_market yes"; "buy $cash / my_market my_market yes" ]
      ; paragraph
          "An action is buy or sell, an amount, a ticker, and the side (yes \
           or no). The amount can be any arithmetic with + - * / and \
           parentheses; it is rounded to whole contracts when the rule \
           fires. An action never stands alone - it needs an if, every, or \
           when i in front of it so the bot knows when to act."
      ; section_title "if statements"
      ; code_block
          [ "if my_market < 0.30 then buy 2 my_market yes else sell 1 \
             my_market yes"
          ]
      ; paragraph
          "if CONDITION then ACTION runs the action whenever the condition \
           holds; the else part is optional. The condition is re-checked at \
           every step of the simulation."
      ; section_title "every statements"
      ; code_block
          [ "every 2h buy 1 my_market yes"
          ; "every 1d if my_market < 0.50 then buy 1 my_market yes"
          ]
      ; paragraph
          "every N throttles a rule to a schedule: N is a number plus m \
           (minutes), h (hours), or d (days). Alone it fires the action on \
           every beat; combined with if it checks the condition only on the \
           beat."
      ; note
          "The every span must be a whole multiple of the simulation \
           interval you picked in configuration - every 90m on an hourly \
           simulation is an error."
      ; section_title "when i statements"
      ; code_block [ "when i buy my_market sell 1 my_market no" ]
      ; paragraph
          "when i buy (or sell) my_market fires right after one of your own \
           trades in that market goes through - useful for hedging or \
           taking the other side automatically. It combines with every and \
           if in one rule: every 1d when i sell my_market buy 1 my_market \
           yes."
      ; section_title "price of, inventory of, avgcost of"
      ; code_block
          [ "if price of my_market > avgcost of my_market then sell 1 \
             my_market yes"
          ; "every 1h buy 10 - inventory of my_market my_market yes"
          ]
      ; paragraph
          "These read your position wherever an amount fits. price of \
           my_market is the yes price (same as the bare ticker; the no \
           price is 1 - my_market). inventory of is the contracts you hold \
           (positive long yes, negative long no), and avgcost of is what \
           you paid per contract on average - so the first rule takes \
           profit, and the second tops your position up to 10. Your account \
           totals are $cash, $realized, and $unrealized."
      ; section_title "variables"
      ; code_block
          [ "lot = 5"
          ; "cheap = my_market < 0.50"
          ; "if $cheap then buy $lot my_market yes"
          ]
      ; paragraph
          "Define a variable bare (name = ...), use it with a $ prefix. A \
           variable can hold an amount or a condition, and must be defined \
           on an earlier line than its first use. Ticker names, keywords, \
           and the built-ins $cash, $realized, and $unrealized are \
           reserved."
      ; section_title "gotchas"
      ; note
          "Tickers are typed in lowercase with every - replaced by _ : the \
           market KXELONMARS-99 is written kxelonmars_99."
      ; note
          "Names may contain hyphens, so put spaces around subtraction: $a \
           - $b, never $a-b."
      ; note
          "= defines a variable; == compares two amounts. There is no \
           comment syntax."
      ]
  in
  Vdom.Node.div
    ~attrs:[ cls "modal-backdrop"; on_click close ]
    [ Vdom.Node.div
      (* Clicks inside the panel must not bubble to the backdrop's close
         handler. *)
        ~attrs:[ cls "modal"; on_click Effect.Stop_propagation ]
        [ Vdom.Node.div
            ~attrs:[ cls "modal-header" ]
            [ Vdom.Node.h2
                ~attrs:[ cls "stage-title" ]
                [ Vdom.Node.text "Bot language manual" ]
            ; button ~class_:"btn-secondary" ~label:"Close" close
            ]
        ; body
        ]
    ]
;;

let rules_view
  ~selected
  ~rules
  ~set_rules
  ~variables
  ~set_variables
  ~draft
  ~set_draft
  ~interval
  ~set_interval
  ~lookback
  ~set_lookback
  ~warmup
  ~set_warmup
  ~starting_cash
  ~set_starting_cash
  ~allow_negative
  ~set_allow_negative
  ~compare_choice
  ~set_compare_choice
  ~saved_bots
  ~bot_count
  ~set_bot_count
  ~bot_probability
  ~set_bot_probability
  ~bot_max_size
  ~set_bot_max_size
  ~error
  ~manual_open
  ~set_manual_open
  ~back
  ~run
  =
  let tickers = List.map selected ~f:(fun (card : Card.t) -> card.slug) in
  let token = Client_logic.Rule_candidates.current_token draft in
  let suggestions =
    match String.is_empty token with
    | true -> []
    | false ->
      Autocomplete.suggest
        (Client_logic.Rule_candidates.env
           ~tickers
           ~variables
           ~program:(String.concat rules ~sep:"\n"))
        ~input:token
  in
  let insert suggestion =
    set_draft (Client_logic.Rule_candidates.complete draft ~with_:suggestion)
  in
  let submit =
    let entry = String.strip draft in
    match String.is_empty entry with
    | true -> Effect.Ignore
    | false ->
      (match Client_logic.Rule_candidates.definition_name entry with
       | Some name ->
         let taken =
           Client_logic.Rule_candidates.builtins
           @ List.filter_map
               variables
               ~f:Client_logic.Rule_candidates.definition_name
         in
         (* A name collision would be a guaranteed parse error at run time;
            refuse it at entry instead. *)
         (match List.mem taken name ~equal:String.equal with
          | true -> Effect.Ignore
          | false ->
            Effect.Many
              [ set_variables (variables @ [ entry ]); set_draft "" ])
       | None -> Effect.Many [ set_rules (rules @ [ entry ]); set_draft "" ])
  in
  let comparison_valid =
    (* Mirrors the server's bounds so Run stays disabled on inputs the server
       would reject. *)
    match (compare_choice : Compare_choice.t) with
    | No_comparison -> true
    | Saved_bot name ->
      (* The bot may have been deleted since it was picked. *)
      Option.is_some (Client_logic.Saved_bot.find saved_bots ~name)
    | Dumb_bots ->
      (match Int.of_string_opt bot_count with
       | Some count -> count >= 1 && count <= 100
       | None -> false)
      && (match Float.of_string_opt bot_probability with
          | Some probability ->
            Float.O.(probability > 0. && probability <= 1.)
          | None -> false)
      &&
        (match Int.of_string_opt bot_max_size with
        | Some size -> size >= 1 && size <= 1000
        | None -> false)
  in
  let lookback_valid =
    (* Mirrors the server's bound: the venue serves at most 5000 candles per
       request, so lookback x interval must fit inside it. *)
    let candles_per_day =
      match (interval : Protocol.Interval.t) with
      | Minute -> 24 * 60
      | Hour -> 24
      | Day -> 1
    in
    match Int.of_string_opt lookback with
    | None -> false
    | Some days -> days >= 1 && days * candles_per_day <= 5000
  in
  let numbers_valid =
    lookback_valid
    && Option.is_some (Int.of_string_opt warmup)
    && comparison_valid
    &&
    match Int.of_string_opt starting_cash with
    | Some cash -> cash > 0
    | None -> false
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
  let labeled label node =
    Vdom.Node.div
      [ Vdom.Node.label
          ~attrs:[ cls "control-label" ]
          [ Vdom.Node.text label ]
      ; node
      ]
  in
  let num_input value set_value =
    Vdom.Node.input
      ~attrs:
        [ cls "num-input"
        ; Vdom.Attr.value value
        ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
            set_value text)
        ]
      ()
  in
  (* One dropdown covers every comparison mode, so picking dumb bots and a
     saved bot at once is impossible by construction. The dumb-bot inputs
     only appear (and are only validated) when that mode runs. *)
  let compare_section =
    let dumb_bots_value = "dumb bots" in
    let choice_option ~value ~label ~chosen =
      Vdom.Node.option
        ~attrs:
          ([ Vdom.Attr.value value ]
           @ match chosen with true -> [ Vdom.Attr.selected ] | false -> [])
        [ Vdom.Node.text label ]
    in
    let choice_options =
      choice_option
        ~value:""
        ~label:"no comparison"
        ~chosen:
          (Compare_choice.equal compare_choice Compare_choice.No_comparison)
      :: choice_option
           ~value:dumb_bots_value
           ~label:dumb_bots_value
           ~chosen:
             (Compare_choice.equal compare_choice Compare_choice.Dumb_bots)
      :: List.map saved_bots ~f:(fun (bot : Client_logic.Saved_bot.t) ->
        (* Prefixed so a bot named "dumb bots" can't shadow the mode. *)
        choice_option
          ~value:[%string "saved:%{bot.name}"]
          ~label:bot.name
          ~chosen:
            (Compare_choice.equal
               compare_choice
               (Compare_choice.Saved_bot bot.name)))
    in
    let dropdown =
      labeled
        "compare against"
        (Vdom.Node.select
           ~attrs:
             [ cls "saved-bot-select"
             ; Vdom.Attr.on_change (fun (_ : _ Js_of_ocaml.Js.t) value ->
                 let choice : Compare_choice.t =
                   match String.chop_prefix value ~prefix:"saved:" with
                   | Some name -> Saved_bot name
                   | None ->
                     (match String.equal value dumb_bots_value with
                      | true -> Dumb_bots
                      | false -> No_comparison)
                 in
                 set_compare_choice choice)
             ]
           choice_options)
    in
    match (compare_choice : Compare_choice.t) with
    | No_comparison | Saved_bot (_ : string) -> [ dropdown ]
    | Dumb_bots ->
      [ dropdown
      ; labeled "number of bots" (num_input bot_count set_bot_count)
      ; labeled
          "trade probability"
          (num_input bot_probability set_bot_probability)
      ; labeled "max trade size" (num_input bot_max_size set_bot_max_size)
      ]
  in
  let config_column =
    Vdom.Node.div
      ~attrs:[ cls "config-col" ]
      ([ Vdom.Node.div
           ~attrs:[ cls "list-heading" ]
           [ Vdom.Node.text "configuration" ]
       ; labeled
           "interval"
           (Vdom.Node.select
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
              (List.map Protocol.Interval.all ~f:interval_option))
       ; labeled "lookback (days)" (num_input lookback set_lookback)
       ; labeled "warmup (hours)" (num_input warmup set_warmup)
       ; labeled
           "starting cash ($)"
           (num_input starting_cash set_starting_cash)
       ; labeled
           "allow negative cash"
           (Vdom.Node.input
              ~attrs:
                [ cls "checkbox"
                ; Vdom.Attr.type_ "checkbox"
                ; Vdom.Attr.bool_property "checked" allow_negative
                ; on_click (set_allow_negative (not allow_negative))
                ]
              ())
       ]
       @ compare_section)
  in
  Vdom.Node.div
    [ Vdom.Node.div
        ~attrs:[ cls "stage-header" ]
        [ Vdom.Node.div
            [ Vdom.Node.h2
                ~attrs:[ cls "stage-title" ]
                [ Vdom.Node.text "Write rules" ]
            ; Vdom.Node.p
                ~attrs:[ cls "stage-hint" ]
                [ Vdom.Node.text
                    "type one rule or variable at a time; click a \
                     suggestion to complete the word you are typing"
                ]
            ]
        ; Vdom.Node.div
            ~attrs:[ cls "button-row" ]
            [ button
                ~class_:"btn-secondary"
                ~label:"? Manual"
                (set_manual_open true)
            ; button ~class_:"btn-secondary" ~label:"Back" back
            ; button
                ~enabled:((not (List.is_empty rules)) && numbers_valid)
                ~class_:"btn-primary"
                ~label:"Run backtest"
                run
            ]
        ]
    ; Vdom.Node.div
        ~attrs:[ cls "chip-row" ]
        [ Vdom.Node.span
            ~attrs:[ cls "chip-row-label" ]
            [ Vdom.Node.text "markets" ]
        ; chips selected
        ]
    ; (match error with
       | None -> Vdom.Node.none
       | Some error ->
         Vdom.Node.div
           ~attrs:[ cls "error-box" ]
           [ Vdom.Node.text (Error.to_string_hum error) ])
    ; Vdom.Node.div
        ~attrs:[ cls "rules-layout" ]
        [ config_column
        ; define_column
            ~draft
            ~set_draft
            ~suggestions
            ~on_pick:insert
            ~submit
        ; rules_column ~rules ~set_rules ~set_draft
        ; variables_column ~tickers ~variables ~set_variables
        ]
    ; (match manual_open with
       | false -> Vdom.Node.none
       | true -> manual_view ~close:(set_manual_open false))
    ]
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

(* Chart x values are epoch seconds (the Protocol wire convention). *)
let date_string epoch_s =
  Time_ns.Span.of_sec epoch_s
  |> Time_ns.of_span_since_epoch
  |> Time_ns.to_date ~zone:Time_float.Zone.utc
  |> Date.to_string
;;

(* Reserved below the plot for the start/end date labels of the x axis. *)
let x_axis_strip_height = 16.

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
  (* A reference line at y = 0, only when the data straddles it. *)
  let zero_line =
    match Float.O.(y_range.lo < 0. && y_range.hi > 0.) with
    | false -> []
    | true ->
      let y = Client_logic.Chart.scale_y y_range ~extent:height 0. in
      [ svg_node
          "line"
          ~attrs:
            [ Vdom.Attr.create "x1" "0"
            ; Vdom.Attr.create "x2" (sprintf "%.0f" width)
            ; Vdom.Attr.create "y1" (sprintf "%.1f" y)
            ; Vdom.Attr.create "y2" (sprintf "%.1f" y)
            ; Vdom.Attr.create "stroke" "#606a82"
            ; Vdom.Attr.create "stroke-width" "1"
            ; Vdom.Attr.create "stroke-dasharray" "2 3"
            ]
          []
      ]
  in
  (* Start and end dates of the plotted extent, in the strip below the plot. *)
  let x_axis_labels =
    match all_points with
    | [] -> []
    | _ :: _ ->
      [ axis_label
          ~x:2.
          ~y:(height +. x_axis_strip_height -. 4.)
          ~anchor:"start"
          (date_string x_range.lo)
      ; axis_label
          ~x:(width -. 2.)
          ~y:(height +. x_axis_strip_height -. 4.)
          ~anchor:"end"
          (date_string x_range.hi)
      ]
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
          ; Vdom.Attr.create
              "height"
              (sprintf "%.0f" (height +. x_axis_strip_height))
          ]
        (warmup_rect
         @ zero_line
         @ polylines
         @ x_axis_labels
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

(* Trades beyond this many start collapsed behind a toggle. *)
let visible_fill_count = 10

let fills_table (fills : Protocol.Fill.t list) ~expanded ~set_expanded =
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
      let reason =
        match fill.reason with
        | None -> Vdom.Node.none
        | Some reason ->
          Vdom.Node.div
            ~attrs:[ cls "fill-reason" ]
            [ Vdom.Node.text reason ]
      in
      Vdom.Node.tr
        [ Vdom.Node.td [ Vdom.Node.text time ]
        ; Vdom.Node.td
            [ Vdom.Node.text
                [%string
                  "#%{fill.id#Int} %{side} %{fill.size#Int} %{contract} on \
                   %{fill.slug#Slug}"]
            ; reason
            ]
        ; Vdom.Node.td ~attrs:[ cls status_class ] [ Vdom.Node.text status ]
        ]
    in
    let total = List.length fills in
    let shown =
      match expanded || total <= visible_fill_count with
      | true -> fills
      | false -> List.take fills visible_fill_count
    in
    let toggle =
      match total <= visible_fill_count with
      | true -> Vdom.Node.none
      | false ->
        let label =
          match expanded with
          | true -> "show fewer"
          | false -> [%string "show all %{total#Int} trades"]
        in
        Vdom.Node.div
          ~attrs:[ cls "fills-toggle" ]
          [ button
              ~class_:"btn-secondary"
              ~label
              (set_expanded (not expanded))
          ]
    in
    Vdom.Node.div
      [ Vdom.Node.table
          ~attrs:[ cls "fills-table" ]
          (Vdom.Node.tr [ header "time"; header "action"; header "status" ]
           :: List.map shown ~f:row)
      ; toggle
      ]
;;

let stat_tile ?value_class ~label ~value () =
  Vdom.Node.div
    ~attrs:[ cls "stat-tile" ]
    [ Vdom.Node.div ~attrs:[ cls "stat-label" ] [ Vdom.Node.text label ]
    ; Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.classes ("stat-value" :: Option.to_list value_class) ]
        [ Vdom.Node.text value ]
    ]
;;

(* Returns and their parts are signed quantities, so their value carries the
   direction color; balances (cash, portfolio, total) stay in plain ink. *)
let signed_stat_tile ~label value =
  let value_class =
    match Float.(value < 0.) with true -> "stat-neg" | false -> "stat-pos"
  in
  stat_tile ~label ~value:(sprintf "$%.2f" value) ~value_class ()
;;

(* What selling every held contract to the market right now would fetch, in
   dollars. The final tick carries the per-market signed holdings and the
   marked yes prices. *)
let portfolio_value
  ~(inventory : (Slug.t * int) list)
  ~(yes_prices : (Slug.t * float) list)
  =
  List.fold yes_prices ~init:0. ~f:(fun acc (slug, price) ->
    let inv = List.Assoc.find_exn inventory ~equal:Slug.equal slug in
    if inv < 0
    then ((abs inv |> Int.to_float) *. (1. -. price)) +. acc
    else ((inv |> Int.to_float) *. price) +. acc)
;;

let ordinal rank =
  let suffix =
    match rank % 100 with
    | 11 | 12 | 13 -> "th"
    | _ ->
      (match rank % 10 with 1 -> "st" | 2 -> "nd" | 3 -> "rd" | _ -> "th")
  in
  [%string "%{rank#Int}%{suffix}"]
;;

let final_stats (final : Protocol.Tick_point.t) ~pnl_percentile =
  let { Protocol.Tick_point.time_s = _
      ; cash
      ; realized
      ; unrealized
      ; yes_prices
      ; inventory
      }
    =
    final
  in
  let portfolio = portfolio_value ~inventory ~yes_prices in
  let money value = sprintf "$%.2f" value in
  let inventory_tile =
    let held = List.filter inventory ~f:(fun (_, count) -> count <> 0) in
    let lines =
      match held with
      | [] -> [ Vdom.Node.div [ Vdom.Node.text "flat" ] ]
      | held ->
        List.map held ~f:(fun (slug, count) ->
          let name = Parser.Ticker_name.normalize (Slug.to_string slug) in
          let count = sprintf "%+d" count in
          Vdom.Node.div [ Vdom.Node.text [%string "%{name} %{count}"] ])
    in
    Vdom.Node.div
      ~attrs:[ cls "stat-tile" ]
      [ Vdom.Node.div
          ~attrs:[ cls "stat-label" ]
          [ Vdom.Node.text "inventory" ]
      ; Vdom.Node.div ~attrs:[ cls "stat-inventory" ] lines
      ]
  in
  let percentile_tile =
    match pnl_percentile with
    | None -> []
    | Some percentile ->
      [ stat_tile
          ~label:"pnl percentile"
          ~value:(ordinal (Float.iround_nearest_exn percentile))
          ()
      ]
  in
  Vdom.Node.div
    ~attrs:[ cls "stats-grid" ]
    ([ stat_tile ~label:"cash" ~value:(money cash) ()
     ; stat_tile ~label:"portfolio value" ~value:(money portfolio) ()
     ; stat_tile ~label:"total value" ~value:(money (cash +. portfolio)) ()
     ; signed_stat_tile ~label:"total returns" (realized +. unrealized)
     ; signed_stat_tile ~label:"realized" realized
     ; signed_stat_tile ~label:"unrealized" unrealized
     ; inventory_tile
     ]
     @ percentile_tile)
;;

(* The baseline point already carries its portfolio value (computed
   server-side), so unlike [final_stats] there is nothing to derive and no
   per-market inventory to show. *)
let baseline_stats
  ({ time_s = _; cash; realized; unrealized; portfolio_value } :
    Protocol.Baseline_point.t)
  =
  let money value = sprintf "$%.2f" value in
  Vdom.Node.div
    ~attrs:[ cls "stats-grid" ]
    [ stat_tile ~label:"avg cash" ~value:(money cash) ()
    ; stat_tile
        ~label:"avg portfolio value"
        ~value:(money portfolio_value)
        ()
    ; stat_tile
        ~label:"avg total value"
        ~value:(money (cash +. portfolio_value))
        ()
    ; signed_stat_tile ~label:"avg total returns" (realized +. unrealized)
    ; signed_stat_tile ~label:"avg realized" realized
    ; signed_stat_tile ~label:"avg unrealized" unrealized
    ]
;;

let results_view
  ~sim_state
  ~fills_expanded
  ~set_fills_expanded
  ~baseline_label
  ~bot_name
  ~set_bot_name
  ~saved_message
  ~save
  ~edit_rules
  ~new_bot
  =
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
      (* The baseline gets its own heading, stat tiles, and charts so the
         averaged dumb-bot runs never mix with the configurable bot's lines;
         absent entirely when the comparison was off. *)
      let baseline_section =
        match List.last result.baseline_ticks with
        | None -> []
        | Some final_baseline ->
          let baseline_line ~name ~color f =
            dashed
              ~name
              ~color
              (List.map
                 result.baseline_ticks
                 ~f:(fun (point : Protocol.Baseline_point.t) ->
                   point.time_s, f point))
          in
          let baseline_pnl =
            [ baseline_line
                ~name:"avg realized"
                ~color:"#4ade80"
                (fun point -> point.realized)
            ; baseline_line
                ~name:"avg unrealized"
                ~color:"#60a5fa"
                (fun point -> point.unrealized)
            ; baseline_line ~name:"avg total" ~color:"#c084fc" (fun point ->
                point.realized +. point.unrealized)
            ]
          in
          let baseline_value =
            [ baseline_line ~name:"avg cash" ~color:"#4ade80" (fun point ->
                point.cash)
            ; baseline_line
                ~name:"avg portfolio"
                ~color:"#60a5fa"
                (fun point -> point.portfolio_value)
            ; baseline_line
                ~name:"avg total value"
                ~color:"#c084fc"
                (fun point -> point.cash +. point.portfolio_value)
            ]
          in
          [ Vdom.Node.div
              ~attrs:[ cls "list-heading" ]
              [ Vdom.Node.text baseline_label ]
          ; baseline_stats final_baseline
          ; Vdom.Node.div
              ~attrs:[ cls "charts-grid" ]
              [ chart_view
                  ~title:
                    [%string
                      "%{baseline_label} pnl (dollars, shaded region is \
                       warmup)"]
                  ~series:baseline_pnl
                  ~sim_start_s:result.sim_start_s
                  ()
              ; chart_view
                  ~title:
                    [%string
                      "%{baseline_label} value (dollars, shaded region is \
                       warmup)"]
                  ~series:baseline_value
                  ~sim_start_s:result.sim_start_s
                  ()
              ]
          ]
      in
      let pnl_series =
        [ solid
            ~name:"realized"
            ~color:"#4ade80"
            (List.map result.ticks ~f:(fun tick ->
               tick.time_s, tick.realized))
        ; solid
            ~name:"unrealized"
            ~color:"#60a5fa"
            (List.map result.ticks ~f:(fun tick ->
               tick.time_s, tick.unrealized))
        ; solid
            ~name:"total"
            ~color:"#c084fc"
            (List.map result.ticks ~f:(fun tick ->
               tick.time_s, tick.realized +. tick.unrealized))
        ]
      in
      let slugs =
        List.concat_map result.ticks ~f:(fun tick ->
          List.map tick.yes_prices ~f:fst)
        |> List.dedup_and_sort ~compare:Slug.compare
      in
      let price_series =
        List.mapi slugs ~f:(fun index slug ->
          solid
            ~name:(Slug.to_string slug)
            ~color:(series_color index)
            (List.filter_map result.ticks ~f:(fun tick ->
               List.Assoc.find tick.yes_prices slug ~equal:Slug.equal
               |> Option.map ~f:(fun price -> tick.time_s, price))))
      in
      let inventory_series =
        List.mapi slugs ~f:(fun index slug ->
          solid
            ~name:(Slug.to_string slug)
            ~color:(series_color index)
            (List.filter_map result.ticks ~f:(fun tick ->
               List.Assoc.find tick.inventory slug ~equal:Slug.equal
               |> Option.map ~f:(fun count ->
                 tick.time_s, Float.of_int count))))
      in
      let value_series =
        let portfolio (tick : Protocol.Tick_point.t) =
          portfolio_value
            ~inventory:tick.inventory
            ~yes_prices:tick.yes_prices
        in
        [ solid
            ~name:"cash"
            ~color:"#4ade80"
            (List.map result.ticks ~f:(fun tick -> tick.time_s, tick.cash))
        ; solid
            ~name:"portfolio value"
            ~color:"#60a5fa"
            (List.map result.ticks ~f:(fun tick ->
               tick.time_s, portfolio tick))
        ; solid
            ~name:"total value"
            ~color:"#c084fc"
            (List.map result.ticks ~f:(fun tick ->
               tick.time_s, tick.cash +. portfolio tick))
        ]
      in
      let stats =
        match Protocol.Sim_result.final result with
        | None -> Vdom.Node.none
        | Some final ->
          final_stats final ~pnl_percentile:result.pnl_percentile
      in
      let truncation_note =
        match result.truncated with
        | false -> Vdom.Node.none
        | true ->
          let starts_at =
            match result.ticks with
            | [] -> ""
            | tick :: _ ->
              [%string " — the simulation starts %{date_string tick.time_s}"]
          in
          Vdom.Node.p
            ~attrs:[ cls "stage-hint" ]
            [ Vdom.Node.text
                [%string
                  "note: the lookback reached past this market's available \
                   history and was truncated%{starts_at}"]
            ]
      in
      Vdom.Node.div
        ([ truncation_note
         ; stats
         ; Vdom.Node.div
             ~attrs:[ cls "charts-grid" ]
             [ chart_view
                 ~title:"pnl (dollars, shaded region is warmup)"
                 ~series:pnl_series
                 ~sim_start_s:result.sim_start_s
                 ()
             ; chart_view
                 ~title:"value (dollars, shaded region is warmup)"
                 ~series:value_series
                 ~sim_start_s:result.sim_start_s
                 ()
             ; chart_view
                 ~title:"market YES prices (dollars)"
                 ~series:price_series
                 ~sim_start_s:result.sim_start_s
                 ()
             ; chart_view
                 ~title:"inventory (contracts held per market)"
                 ~series:inventory_series
                 ~sim_start_s:result.sim_start_s
                 ()
             ]
         ]
         @ baseline_section
         @ [ fills_table
               result.fills
               ~expanded:fills_expanded
               ~set_expanded:set_fills_expanded
           ])
  in
  Vdom.Node.div
    [ Vdom.Node.h2 ~attrs:[ cls "stage-title" ] [ Vdom.Node.text "Results" ]
    ; body
    ; Vdom.Node.div
        ~attrs:[ cls "button-row" ]
        [ button ~class_:"btn-secondary" ~label:"Edit rules" edit_rules
        ; button ~class_:"btn-secondary" ~label:"New bot" new_bot
        ; Vdom.Node.input
            ~attrs:
              [ cls "bot-name-input"
              ; Vdom.Attr.placeholder "bot name"
              ; Vdom.Attr.value bot_name
              ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
                  set_bot_name text)
              ]
            ()
        ; button
            ~enabled:(not (String.is_empty (String.strip bot_name)))
            ~class_:"btn-secondary"
            ~label:"Save bot"
            save
        ; (match saved_message with
           | None -> Vdom.Node.none
           | Some message ->
             Vdom.Node.span
               ~attrs:[ cls "save-confirm" ]
               [ Vdom.Node.text [%string "✓ %{message}"] ])
        ]
    ]
;;

let bots_page markets_result (local_ graph) =
  let stage, set_stage = Bonsai.state Stage.Pick graph in
  let selected, set_selected = Bonsai.state ([] : Card.t list) graph in
  let search_text, set_search_text = Bonsai.state "" graph in
  let rules, set_rules = Bonsai.state ([] : string list) graph in
  let variables, set_variables = Bonsai.state ([] : string list) graph in
  let draft, set_draft = Bonsai.state "" graph in
  let interval, set_interval = Bonsai.state Protocol.Interval.Hour graph in
  let lookback, set_lookback = Bonsai.state "14" graph in
  let warmup, set_warmup = Bonsai.state "12" graph in
  let starting_cash, set_starting_cash = Bonsai.state "100" graph in
  let allow_negative, set_allow_negative = Bonsai.state false graph in
  let compare_choice, set_compare_choice =
    Bonsai.state Compare_choice.No_comparison graph
  in
  let baseline_label, set_baseline_label = Bonsai.state "" graph in
  let bot_count, set_bot_count = Bonsai.state "5" graph in
  let bot_probability, set_bot_probability = Bonsai.state "0.2" graph in
  let bot_max_size, set_bot_max_size = Bonsai.state "100" graph in
  let sim_state, set_sim_state = Bonsai.state Sim_state.Idle graph in
  let fills_expanded, set_fills_expanded = Bonsai.state false graph in
  let error, set_error = Bonsai.state (None : Error.t option) graph in
  let manual_open, set_manual_open = Bonsai.state false graph in
  let saved_bots, set_saved_bots = Bonsai.state (load_saved_bots ()) graph in
  let bot_name, set_bot_name = Bonsai.state "" graph in
  let picked_bot, set_picked_bot = Bonsai.state "" graph in
  let saved_message, set_saved_message =
    Bonsai.state (None : string option) graph
  in
  let dispatch_sim =
    Rpc_effect.Rpc.dispatcher Protocol.run_simulation graph
  in
  let%arr stage
  and set_stage
  and selected
  and set_selected
  and search_text
  and set_search_text
  and rules
  and set_rules
  and variables
  and set_variables
  and draft
  and set_draft
  and interval
  and set_interval
  and lookback
  and set_lookback
  and warmup
  and set_warmup
  and starting_cash
  and set_starting_cash
  and allow_negative
  and set_allow_negative
  and compare_choice
  and set_compare_choice
  and baseline_label
  and set_baseline_label
  and bot_count
  and set_bot_count
  and bot_probability
  and set_bot_probability
  and bot_max_size
  and set_bot_max_size
  and sim_state
  and set_sim_state
  and fills_expanded
  and set_fills_expanded
  and error
  and set_error
  and manual_open
  and set_manual_open
  and saved_bots
  and set_saved_bots
  and bot_name
  and set_bot_name
  and picked_bot
  and set_picked_bot
  and saved_message
  and set_saved_message
  and dispatch_sim
  and markets_result in
  let all_cards =
    match markets_result with
    | Some (Ok cards) -> cards
    | Some (Error (_ : Error.t)) | None -> []
  in
  match (stage : Stage.t) with
  | Pick ->
    (* Restoring a saved bot looks its slugs back up in today's catalog;
       markets the server has rotated out since the save are dropped. *)
    let load_bot (bot : Client_logic.Saved_bot.t) =
      let cards =
        List.filter_map bot.slugs ~f:(fun slug ->
          List.find all_cards ~f:(fun (card : Card.t) ->
            Slug.equal card.slug slug))
      in
      let interval_effects =
        match
          List.find Protocol.Interval.all ~f:(fun value ->
            String.equal (Protocol.Interval.name value) bot.interval)
        with
        | None -> []
        | Some value -> [ set_interval value ]
      in
      Effect.Many
        ([ set_selected cards
         ; set_rules bot.rules
         ; set_variables bot.variables
         ; set_lookback bot.lookback
         ; set_warmup bot.warmup
         ; set_starting_cash bot.starting_cash
         ; set_allow_negative bot.allow_negative
         ; set_bot_name bot.name
         ]
         @ interval_effects)
    in
    let delete_bot name =
      let bots = Client_logic.Saved_bot.remove saved_bots ~name in
      Effect.Many
        [ store_saved_bots_effect bots
        ; set_saved_bots bots
        ; set_picked_bot ""
        ]
    in
    pick_view
      ~all_cards
      ~selected
      ~set_selected
      ~search_text
      ~set_search_text
      ~saved_bots
      ~picked_bot
      ~set_picked_bot
      ~load_bot
      ~delete_bot
      ~continue:(set_stage Rules)
  | Rules ->
    let run =
      (* [None] means the comparison inputs don't parse (or the saved bot is
         gone); the Run button is disabled on the same condition. *)
      let comparison_request : Protocol.Comparison.t option =
        match (compare_choice : Compare_choice.t) with
        | No_comparison -> Some Protocol.Comparison.No_comparison
        | Dumb_bots ->
          (match
             ( Int.of_string_opt bot_count
             , Float.of_string_opt bot_probability
             , Int.of_string_opt bot_max_size )
           with
           | Some count, Some trade_probability, Some max_size ->
             Some (Dumb_bots { count; trade_probability; max_size })
           | None, _, _ | _, None, _ | _, _, None -> None)
        | Saved_bot name ->
          (match Client_logic.Saved_bot.find saved_bots ~name with
           | None -> None
           | Some bot ->
             Some
               (Rival_bot
                  { name = bot.name
                  ; slugs = bot.slugs
                  ; program = String.concat bot.rules ~sep:"\n"
                  ; variables = bot.variables
                  }))
      in
      match
        ( Int.of_string_opt lookback
        , Int.of_string_opt warmup
        , Int.of_string_opt starting_cash
        , comparison_request )
      with
      | ( Some lookback_days
        , Some warmup_hours
        , Some starting_cash_dollars
        , Some comparison ) ->
        let request : Protocol.Sim_request.t =
          { slugs = List.map selected ~f:(fun card -> card.Card.slug)
          ; program = String.concat rules ~sep:"\n"
          ; variables
          ; interval
          ; lookback_days
          ; warmup_hours
          ; starting_cash_cents = starting_cash_dollars * 100
          ; allow_negative_cash = allow_negative
          ; comparison
          }
        in
        (* Captured at run time so the Results labels describe this run even
           if the dropdown changes afterwards. *)
        let label =
          match (compare_choice : Compare_choice.t) with
          | No_comparison -> ""
          | Dumb_bots -> "dumb bot average"
          | Saved_bot name -> [%string "saved bot %{name}"]
        in
        let open Effect.Let_syntax in
        let%bind () =
          Effect.Many
            [ set_error None
            ; set_sim_state Running
            ; set_fills_expanded false
            ; set_baseline_label label
            ; set_stage Results
            ]
        in
        let%bind response = dispatch_sim request in
        (match Or_error.join response with
         | Ok result -> set_sim_state (Done result)
         | Error error ->
           (* Bring the user back to their rules with the error inline. *)
           Effect.Many
             [ set_sim_state Idle; set_error (Some error); set_stage Rules ])
      | None, _, _, _ | _, None, _, _ | _, _, None, _ | _, _, _, None ->
        Effect.Ignore
    in
    rules_view
      ~selected
      ~rules
      ~set_rules
      ~variables
      ~set_variables
      ~draft
      ~set_draft
      ~interval
      ~set_interval
      ~lookback
      ~set_lookback
      ~warmup
      ~set_warmup
      ~starting_cash
      ~set_starting_cash
      ~allow_negative
      ~set_allow_negative
      ~compare_choice
      ~set_compare_choice
      ~saved_bots
      ~bot_count
      ~set_bot_count
      ~bot_probability
      ~set_bot_probability
      ~bot_max_size
      ~set_bot_max_size
      ~error
      ~manual_open
      ~set_manual_open
      ~back:(set_stage Pick)
      ~run
  | Results ->
    let save =
      let name = String.strip bot_name in
      match String.is_empty name with
      | true -> Effect.Ignore
      | false ->
        let bot : Client_logic.Saved_bot.t =
          { name
          ; slugs = List.map selected ~f:(fun (card : Card.t) -> card.slug)
          ; rules
          ; variables
          ; interval = Protocol.Interval.name interval
          ; lookback
          ; warmup
          ; starting_cash
          ; allow_negative
          }
        in
        let bots = Client_logic.Saved_bot.upsert saved_bots bot in
        Effect.Many
          [ store_saved_bots_effect bots
          ; set_saved_bots bots
          ; set_saved_message (Some [%string "saved as %{name}"])
          ]
    in
    (* Typing a new name invalidates the "saved as ..." confirmation. *)
    let set_bot_name text =
      Effect.Many [ set_bot_name text; set_saved_message None ]
    in
    results_view
      ~sim_state
      ~fills_expanded
      ~set_fills_expanded
      ~baseline_label
      ~bot_name
      ~set_bot_name
      ~saved_message
      ~save
      ~edit_rules:(Effect.Many [ set_saved_message None; set_stage Rules ])
      ~new_bot:
        (Effect.Many
           [ set_selected []
           ; set_rules []
           ; set_variables []
           ; set_draft ""
           ; set_bot_name ""
           ; set_saved_message None
           ; set_sim_state Idle
           ; set_fills_expanded false
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

(* ---------- Market detail popup ---------- *)

module Detail_state = struct
  type t =
    | Idle
    | Loading
    | Failed of Error.t
    | Done of Protocol.Market_detail.t
end

let seconds_per_day = 86_400.

let market_detail_modal (card : Card.t) (detail : Detail_state.t) ~close =
  let body =
    match (detail : Detail_state.t) with
    | Idle | Loading ->
      Vdom.Node.p
        ~attrs:[ cls "status" ]
        [ Vdom.Node.text "loading price history..." ]
    | Failed error ->
      Vdom.Node.div
        ~attrs:[ cls "error-box" ]
        [ Vdom.Node.text (Error.to_string_hum error) ]
    | Done { prices; volumes } ->
      let trailing_volumes =
        Client_logic.Market_stats.trailing_sum
          ~window_s:seconds_per_day
          volumes
      in
      let current_price_tile =
        match List.last prices with
        | None -> []
        | Some (_, price) ->
          [ stat_tile
              ~label:"current price"
              ~value:(sprintf "$%.2f" price)
              ()
          ]
      in
      let volume_24h_tile =
        match List.last trailing_volumes with
        | None -> []
        | Some (_, volume) ->
          [ stat_tile
              ~label:"24h volume"
              ~value:(sprintf "%.0f contracts" volume)
              ()
          ]
      in
      let stats =
        Vdom.Node.div
          ~attrs:[ cls "stats-grid" ]
          (current_price_tile
           @ [ stat_tile
                 ~label:"volatility"
                 ~value:
                   (sprintf
                      "$%.3f"
                      (Client_logic.Market_stats.volatility
                         (List.map prices ~f:snd)))
                 ()
             ; stat_tile ~label:"total volume" ~value:(volume_string card) ()
             ]
           @ volume_24h_tile)
      in
      let price_chart =
        chart_view
          ~title:"yes price (dollars)"
          ~series:[ solid ~name:"yes price" ~color:"#60a5fa" prices ]
          ()
      in
      let volume_chart =
        match trailing_volumes with
        | [] -> []
        | _ :: _ ->
          [ chart_view
              ~title:"24h volume (contracts)"
              ~series:
                [ solid ~name:"24h volume" ~color:"#4ade80" trailing_volumes
                ]
              ()
          ]
      in
      Vdom.Node.div ([ stats; price_chart ] @ volume_chart)
  in
  Vdom.Node.div
    ~attrs:[ cls "modal-backdrop"; on_click close ]
    [ Vdom.Node.div
      (* Clicks inside the panel must not bubble to the backdrop's close
         handler. *)
        ~attrs:[ cls "modal"; on_click Effect.Stop_propagation ]
        [ Vdom.Node.div
            ~attrs:[ cls "modal-header" ]
            [ Vdom.Node.h2
                ~attrs:[ cls "stage-title" ]
                [ Vdom.Node.text card.title ]
            ; button ~class_:"btn-secondary" ~label:"Close" close
            ]
        ; body
        ]
    ]
;;

let markets_page markets_result (local_ graph) =
  let selected, set_selected = Bonsai.state (None : Card.t option) graph in
  let detail, set_detail = Bonsai.state Detail_state.Idle graph in
  let dispatch_detail =
    Rpc_effect.Rpc.dispatcher Protocol.get_market_detail graph
  in
  let%arr selected
  and set_selected
  and detail
  and set_detail
  and dispatch_detail
  and markets_result in
  let close =
    Effect.Many [ set_selected None; set_detail Detail_state.Idle ]
  in
  let on_select (card : Card.t) =
    let open Effect.Let_syntax in
    let%bind () =
      Effect.Many
        [ set_selected (Some card); set_detail Detail_state.Loading ]
    in
    let%bind response = dispatch_detail card.slug in
    match Or_error.join response with
    | Ok detail -> set_detail (Done detail)
    | Error error -> set_detail (Failed error)
  in
  let modal =
    match selected with
    | None -> Vdom.Node.none
    | Some card -> market_detail_modal card detail ~close
  in
  Vdom.Node.div [ markets_page_view markets_result ~on_select; modal ]
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
  let markets_result = Markets_page.fetch_markets graph in
  (* Only the active page's computation is alive: [match%sub] deactivates
     the other branches (their models are retained, so wizard state
     survives page flips) and [Bonsai.delay] defers even building a page's
     graph until its first visit. Re-entering a page re-fires its
     [on_activate] refresh — matching the arb page's own
     refresh-on-section-entry behavior. *)
  let body =
    match%sub page with
    | Markets -> Markets_page.markets_page markets_result graph
    | Bots ->
      Bonsai.delay graph ~f:(fun graph ->
        Bots_page.bots_page markets_result graph)
    | Arbitrage ->
      Bonsai.delay graph ~f:(fun graph -> Arb_page.arbitrage_page graph)
    | Wallet ->
      Bonsai.delay graph ~f:(fun graph -> Wallet_page.wallet_page graph)
  in
  let%arr page
  and set_page
  and body in
  Vdom.Node.div
    [ Vdom.Node.div
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
