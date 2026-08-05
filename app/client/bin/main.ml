(* The web app's client half: a Bonsai single-page app served by
   [app/server]. The pages live beside this file — {!Markets_page},
   {!Bots_page}, {!Arb_page}, {!Wallet_page} — over the shared view
   helpers in {!Ui} and the chart renderer in {!Chart_view}. This
   file holds the stylesheet, the page enum, and the shell that
   swaps pages. *)

open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Ui

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
  .wallet-disclaimer {
    background: #2b2113; border: 1px solid #7c5806; border-radius: 10px;
    padding: 12px 16px; margin: 0 0 16px; font-size: 13px; color: #fcd34d;
    max-width: 720px;
  }
  .wallet-disclaimer p { margin: 0 0 8px; }
  .wallet-disclaimer p:last-child { margin-bottom: 0; }
  .wallet-form { display: flex; flex-direction: column; gap: 10px; max-width: 720px; }
  .wallet-row { display: flex; flex-direction: column; gap: 4px; }
  .wallet-label { font-size: 12px; color: #8b93a7; }
  .wallet-pem {
    background: #141a26; border: 1px solid #2a3040; border-radius: 8px;
    color: #e8eaf0; padding: 8px 10px;
    font-family: ui-monospace, monospace; font-size: 12px;
    min-height: 130px; width: 100%; box-sizing: border-box;
  }
  .wallet-hint { font-size: 12px; color: #8b93a7; }
  .wallet-mismatch { font-size: 12px; color: #f87171; }
  .wallet-prod-warning { color: #f87171; font-size: 12px; }
  .wallet-status-card {
    background: #141a26; border: 1px solid #2a3040; border-radius: 12px;
    padding: 14px 18px; margin: 0 0 14px; max-width: 720px;
  }
  .wallet-key-hint { font-family: ui-monospace, monospace; font-size: 15px; }
  .wallet-badge {
    font-size: 11px; padding: 2px 10px; border-radius: 999px;
    border: 1px solid #2a3040; margin-left: 8px; white-space: nowrap;
  }
  .wallet-badge-demo { color: #7dd3fc; border-color: #155e75; }
  .wallet-badge-prod { color: #f87171; border-color: #7f1d1d; }
  .wallet-state-line { margin: 8px 0 12px; font-size: 13px; color: #a5b4d0; }
  .wallet-unlocked { color: #4ade80; }
  .wallet-locked { color: #fbbf24; }
|}
;;

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
  let markets = Markets_page.markets_page markets_result graph in
  let bots = Bots_page.bots_page markets_result graph in
  let arbitrage = Arb_page.arbitrage_page graph in
  let wallet = Wallet_page.wallet_page graph in
  let%arr page
  and set_page
  and markets
  and bots
  and arbitrage
  and wallet in
  let body =
    match page with
    | Markets -> markets
    | Bots -> bots
    | Arbitrage -> arbitrage
    | Wallet -> wallet
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
