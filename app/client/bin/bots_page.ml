(* The Bots page: the three-stage backtest wizard (pick markets → write rules
   → results) with its stat tiles, fills table, and result charts. *)

open! Core
open! Types
open Bonsai_web
open Bonsai.Let_syntax
open Ui
open Chart_view
module Card = Protocol.Market_card

let max_markets = 4

(* Downsampling budget per chart series (~device pixels across the plot
   area); a minute-interval month is ~43k ticks without it. *)
let chart_point_budget = 800

(* ---------- Bots page ---------- *)

module Stage = struct
  type t =
    | Pick
    | Rules
    | Results
  [@@deriving sexp_of, equal]
end

module Sim_state = struct
  (* [Done] carries the chart series assembled (and downsampled) once at
     completion — results re-renders (hover, fills toggle) must never re-walk
     the raw ticks. *)
  type t =
    | Idle
    | Running
    | Done of Protocol.Sim_result.t * Client_logic.Sim_series.t
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

(* Column 2: one input takes either statement kind — a [name = expr] line
   becomes a variable, anything else a rule — one per Enter press. Nothing
   compiles until Run; the server reports parse errors then. *)
let define_column ~draft ~set_draft ~suggestions ~on_pick ~submit ~collision =
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
    ; field_error collision
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
  (* Field bounds mirror the server's ([Form_validate]), each with its own
     message under the input — Run stays disabled on invalid input, but now
     the user is told which field is wrong and why. *)
  let lookback_error =
    Result.error (Client_logic.Form_validate.lookback_days lookback)
  in
  let warmup_error =
    match Client_logic.Form_validate.warmup_hours warmup with
    | Error message -> Some message
    | Ok warmup_hours ->
      (match Client_logic.Form_validate.lookback_days lookback with
       | Error (_ : string) -> None
       | Ok lookback_days ->
         Client_logic.Form_validate.warmup_within_lookback
           ~warmup_hours
           ~lookback_days)
  in
  let cash_error =
    Result.error
      (Client_logic.Form_validate.starting_cash_dollars starting_cash)
  in
  let bot_count_error, probability_error, max_size_error =
    match (compare_choice : Compare_choice.t) with
    | No_comparison | Saved_bot (_ : string) -> None, None, None
    | Dumb_bots ->
      ( Result.error (Client_logic.Form_validate.bot_count bot_count)
      , Result.error
          (Client_logic.Form_validate.trade_probability bot_probability)
      , Result.error (Client_logic.Form_validate.bot_max_size bot_max_size) )
  in
  (* The picked rival may have been deleted from storage since. *)
  let rival_error =
    match (compare_choice : Compare_choice.t) with
    | No_comparison | Dumb_bots -> None
    | Saved_bot name ->
      (match Client_logic.Saved_bot.find saved_bots ~name with
       | Some (_ : Client_logic.Saved_bot.t) -> None
       | None -> Some [%string "saved bot %{name} no longer exists"])
  in
  let collision =
    Client_logic.Form_validate.definition_collision ~draft ~variables
  in
  let numbers_valid =
    List.for_all
      [ lookback_error
      ; warmup_error
      ; cash_error
      ; bot_count_error
      ; probability_error
      ; max_size_error
      ; rival_error
      ]
      ~f:Option.is_none
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
        (Vdom.Node.div
           [ Vdom.Node.select
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
               choice_options
           ; field_error rival_error
           ])
    in
    match (compare_choice : Compare_choice.t) with
    | No_comparison | Saved_bot (_ : string) -> [ dropdown ]
    | Dumb_bots ->
      [ dropdown
      ; labeled
          "number of bots"
          (num_input
             ?error:bot_count_error
             ~value:bot_count
             ~set_value:set_bot_count
             ())
      ; labeled
          "trade probability"
          (num_input
             ?error:probability_error
             ~value:bot_probability
             ~set_value:set_bot_probability
             ())
      ; labeled
          "max trade size"
          (num_input
             ?error:max_size_error
             ~value:bot_max_size
             ~set_value:set_bot_max_size
             ())
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
       ; labeled
           "lookback (days)"
           (num_input
              ?error:lookback_error
              ~value:lookback
              ~set_value:set_lookback
              ())
       ; labeled
           "warmup (hours)"
           (num_input
              ?error:warmup_error
              ~value:warmup
              ~set_value:set_warmup
              ())
       ; labeled
           "starting cash ($)"
           (num_input
              ?error:cash_error
              ~value:starting_cash
              ~set_value:set_starting_cash
              ())
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
            ~collision
        ; rules_column ~rules ~set_rules ~set_draft
        ; variables_column ~tickers ~variables ~set_variables
        ]
    ; (match manual_open with
       | false -> Vdom.Node.none
       | true -> manual_view ~close:(set_manual_open false))
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
      [ Vdom.Node.div
        (* Narrow screens scroll the table inside this wrapper instead of
           stretching the page. *)
          ~attrs:[ cls "fills-table-wrap" ]
          [ Vdom.Node.table
              ~attrs:[ cls "fills-table" ]
              (Vdom.Node.tr
                 [ header "time"; header "action"; header "status" ]
               :: List.map shown ~f:row)
          ]
      ; toggle
      ]
;;

(* What selling every held contract to the market right now would fetch, in
   dollars. The final tick carries the per-market signed holdings and the
   marked yes prices. *)
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
  let portfolio = Client_logic.Portfolio.value ~inventory ~yes_prices in
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
  ~hover
  ~set_hover
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
    | Done (result, series) ->
      (* Series come pre-assembled and downsampled from [Sim_series]; this
         layer only assigns chart classes and threads the hover cell. *)
      let to_series ~dash named =
        List.mapi
          named
          ~f:(fun index { Client_logic.Sim_series.Series.name; points } ->
            match dash with
            | false -> solid ~name ~index points
            | true -> dashed ~name ~index points)
      in
      let chart ~title ~dash named =
        chart_view
          ~title
          ~series:(to_series ~dash named)
          ~sim_start_s:result.sim_start_s
          ~hover
          ~set_hover
          ()
      in
      (* The baseline gets its own heading, stat tiles, and charts so the
         averaged dumb-bot runs never mix with the configurable bot's lines;
         absent entirely when the comparison was off. *)
      let baseline_section =
        match List.last result.baseline_ticks with
        | None -> []
        | Some final_baseline ->
          [ Vdom.Node.div
              ~attrs:[ cls "list-heading" ]
              [ Vdom.Node.text baseline_label ]
          ; baseline_stats final_baseline
          ; Vdom.Node.div
              ~attrs:[ cls "charts-grid" ]
              [ chart
                  ~title:
                    [%string
                      "%{baseline_label} pnl (dollars, shaded region is \
                       warmup)"]
                  ~dash:false
                  series.baseline_pnl
              ; chart
                  ~title:
                    [%string
                      "%{baseline_label} value (dollars, shaded region is \
                       warmup)"]
                  ~dash:false
                  series.baseline_value
              ]
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
             [ chart
                 ~title:"pnl (dollars, shaded region is warmup)"
                 ~dash:false
                 series.pnl
             ; chart
                 ~title:"value (dollars, shaded region is warmup)"
                 ~dash:false
                 series.value
             ; chart
                 ~title:"market YES prices (dollars)"
                 ~dash:false
                 series.prices
             ; chart
                 ~title:"inventory (contracts held per market)"
                 ~dash:false
                 series.inventory
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
  (* One hover cell for every results chart, keyed by chart title. *)
  let chart_hover, set_chart_hover = Bonsai.state_opt graph in
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
  and chart_hover
  and set_chart_hover
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
         | Ok result ->
           set_sim_state
             (Done
                ( result
                , Client_logic.Sim_series.create
                    result
                    ~max_points:chart_point_budget ))
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
      ~hover:chart_hover
      ~set_hover:set_chart_hover
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
