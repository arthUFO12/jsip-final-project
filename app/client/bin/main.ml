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
       Vdom.Node.div
         ~attrs:[ cls "chip" ]
         (Vdom.Node.text (Slug.to_string card.slug) :: remove)))
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
  ~compare_bots
  ~set_compare_bots
  ~bot_count
  ~set_bot_count
  ~bot_probability
  ~set_bot_probability
  ~bot_max_size
  ~set_bot_max_size
  ~error
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
  let basic_bots_valid =
    (* Mirrors the server's bounds so Run stays disabled on inputs the server
       would reject. *)
    match compare_bots with
    | false -> true
    | true ->
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
  let numbers_valid =
    Option.is_some (Int.of_string_opt lookback)
    && Option.is_some (Int.of_string_opt warmup)
    && basic_bots_valid
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
  (* Collapsed to just the toggle until it's on; the inputs only matter (and
     are only validated) when the comparison runs. *)
  let compare_section =
    let toggle =
      labeled
        "compare against dumb bots"
        (Vdom.Node.input
           ~attrs:
             [ cls "checkbox"
             ; Vdom.Attr.type_ "checkbox"
             ; Vdom.Attr.bool_property "checked" compare_bots
             ; on_click (set_compare_bots (not compare_bots))
             ]
           ())
    in
    match compare_bots with
    | false -> [ toggle ]
    | true ->
      [ toggle
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
            [ button ~class_:"btn-secondary" ~label:"Back" back
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

let chart_view ~title ~(series : Chart_series.t list) ~sim_start_s =
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
  let warmup_width =
    Client_logic.Chart.scale_x x_range ~extent:width sim_start_s
    |> Float.clamp_exn ~min:0. ~max:width
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
              [ Vdom.Node.text "dumb bot average" ]
          ; baseline_stats final_baseline
          ; Vdom.Node.div
              ~attrs:[ cls "charts-grid" ]
              [ chart_view
                  ~title:
                    "dumb bot avg pnl (dollars, shaded region is warmup)"
                  ~series:baseline_pnl
                  ~sim_start_s:result.sim_start_s
              ; chart_view
                  ~title:
                    "dumb bot avg value (dollars, shaded region is warmup)"
                  ~series:baseline_value
                  ~sim_start_s:result.sim_start_s
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
      Vdom.Node.div
        ([ stats
         ; Vdom.Node.div
             ~attrs:[ cls "charts-grid" ]
             [ chart_view
                 ~title:"pnl (dollars, shaded region is warmup)"
                 ~series:pnl_series
                 ~sim_start_s:result.sim_start_s
             ; chart_view
                 ~title:"value (dollars, shaded region is warmup)"
                 ~series:value_series
                 ~sim_start_s:result.sim_start_s
             ; chart_view
                 ~title:"market YES prices (dollars)"
                 ~series:price_series
                 ~sim_start_s:result.sim_start_s
             ; chart_view
                 ~title:"inventory (contracts held per market)"
                 ~series:inventory_series
                 ~sim_start_s:result.sim_start_s
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
  let compare_bots, set_compare_bots = Bonsai.state false graph in
  let bot_count, set_bot_count = Bonsai.state "5" graph in
  let bot_probability, set_bot_probability = Bonsai.state "0.2" graph in
  let bot_max_size, set_bot_max_size = Bonsai.state "100" graph in
  let sim_state, set_sim_state = Bonsai.state Sim_state.Idle graph in
  let fills_expanded, set_fills_expanded = Bonsai.state false graph in
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
  and compare_bots
  and set_compare_bots
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
      let basic_bots_request =
        match compare_bots with
        | false -> Some None
        | true ->
          (match
             ( Int.of_string_opt bot_count
             , Float.of_string_opt bot_probability
             , Int.of_string_opt bot_max_size )
           with
           | Some count, Some trade_probability, Some max_size ->
             Some
               (Some
                  ({ count; trade_probability; max_size }
                   : Protocol.Basic_bots.t))
           | None, _, _ | _, None, _ | _, _, None -> None)
      in
      match
        ( Int.of_string_opt lookback
        , Int.of_string_opt warmup
        , Int.of_string_opt starting_cash
        , basic_bots_request )
      with
      | ( Some lookback_days
        , Some warmup_hours
        , Some starting_cash_dollars
        , Some basic_bots ) ->
        let request : Protocol.Sim_request.t =
          { slugs = List.map selected ~f:(fun card -> card.Card.slug)
          ; program = String.concat rules ~sep:"\n"
          ; variables
          ; interval
          ; lookback_days
          ; warmup_hours
          ; starting_cash_cents = starting_cash_dollars * 100
          ; allow_negative_cash = allow_negative
          ; basic_bots
          }
        in
        let open Effect.Let_syntax in
        let%bind () =
          Effect.Many
            [ set_error None
            ; set_sim_state Running
            ; set_fills_expanded false
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
      ~compare_bots
      ~set_compare_bots
      ~bot_count
      ~set_bot_count
      ~bot_probability
      ~set_bot_probability
      ~bot_max_size
      ~set_bot_max_size
      ~error
      ~back:(set_stage Pick)
      ~run
  | Results ->
    results_view
      ~sim_state
      ~fills_expanded
      ~set_fills_expanded
      ~edit_rules:(set_stage Rules)
      ~new_bot:
        (Effect.Many
           [ set_selected []
           ; set_rules []
           ; set_variables []
           ; set_draft ""
           ; set_sim_state Idle
           ; set_fills_expanded false
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
