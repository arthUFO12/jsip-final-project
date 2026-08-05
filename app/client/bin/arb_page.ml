(* The Arbitrage page: the sweep → review → arb-pairs pipeline, the
   paper/real wallet scoreboard, and the assisted-hedge dialog. *)

open! Core
open! Types
open Bonsai_web
open Bonsai.Let_syntax
open Ui
open Chart_view

(* ---------- Arbitrage page ---------- *)

(* Every background request on this page shares {!Ui.Request_status}, so
   each has a [Failed] arm its panel must render — sweep, scan, and
   auto-review used to map failures back to [Idle], silently. *)
module Sweep_state = struct
  type t = Protocol.Sweep_summary.t Request_status.t [@@deriving sexp_of]
end

module Scan_state = struct
  type t = Protocol.Scan_report.t Request_status.t [@@deriving sexp_of]
end

module Llm_state = struct
  type t = Protocol.Llm_review_summary.t Request_status.t
  [@@deriving sexp_of]
end

module Auto_state = struct
  type t = Protocol.Auto_review_summary.t Request_status.t
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
    | Backtest
  [@@deriving sexp_of, compare, equal, enumerate]

  let stage = function
    | Sweep -> "1", "Sweep", "scrape both venues and text-match every title"
    | Review ->
      "2", "Review", "LLM or human: approve pairs settling on the same event"
    | Pairs -> "3", "Arb Pairs", "price approved pairs on live order books"
    | Backtest ->
      "4", "Backtest", "replay the strategy over the pairs' price history"
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
  let running = Request_status.is_running sweep_state in
  let threshold_error =
    Result.error (Client_logic.Form_validate.match_threshold threshold)
  in
  let status =
    match (sweep_state : Sweep_state.t) with
    | Idle -> Vdom.Node.div []
    | Failed error -> error_banner ~retry:run error
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
        ; num_input
            ?error:threshold_error
            ~value:threshold
            ~set_value:set_threshold
            ()
        ; button
            ~enabled:((not running) && Option.is_none threshold_error)
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
  let running = Request_status.is_running auto_state in
  let threshold_error =
    Result.error
      (Client_logic.Form_validate.match_threshold auto_threshold)
  in
  let status =
    match (auto_state : Auto_state.t) with
    | Idle -> Vdom.Node.div []
    | Failed error -> error_banner ~retry:run error
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
        ; num_input
            ?error:threshold_error
            ~value:auto_threshold
            ~set_value:set_auto_threshold
            ()
        ; button
            ~enabled:((not running) && Option.is_none threshold_error)
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
  let running = Request_status.is_running llm_state in
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
    | Failed error -> error_banner ~retry:run error
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
    | Some (Error error) -> error_box error
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
    Vdom.Node.div ~attrs:[ cls "arb-wallet" ] [ error_box error ]
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
    let price_error =
      Result.error (Client_logic.Form_validate.fill_price_cents price_text)
    in
    let count_error =
      Result.error (Client_logic.Form_validate.fill_count count_text)
    in
    let fields_valid =
      Option.is_none price_error && Option.is_none count_error
    in
    box
      [ Vdom.Node.h2
          ~attrs:[ cls "arb-panel-title" ]
          [ Vdom.Node.text "Assisted hedge — step 2: confirm your fill" ]
      ; Vdom.Node.div
          ~attrs:[ cls "arb-controls" ]
          [ Vdom.Node.label
              ~attrs:[ cls "control-label" ]
              [ Vdom.Node.text "your fill price (cents)" ]
          ; num_input
              ?error:price_error
              ~value:price_text
              ~set_value:set_price
              ()
          ; Vdom.Node.label
              ~attrs:[ cls "control-label" ]
              [ Vdom.Node.text "contracts filled" ]
          ; num_input
              ?error:count_error
              ~value:count_text
              ~set_value:set_count
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
              ~enabled:((not firing) && fields_valid)
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

(* The strategy runner's cockpit: start/stop the server-side loop, watch
   its ticks, and read the forward paper PnL accrued from real scans. The
   armed path is the page's only standing authorization to spend — its
   copy stays sober and spells out the one-sidedness. *)
let arb_live_runner_card
  ~(live_run : Protocol.Live_run.Status.t Or_error.t option)
  ~interval
  ~set_interval
  ~arm
  ~set_arm
  ~budget
  ~set_budget
  ~start
  ~stop
  ~refresh
  ~history
  ~hover
  ~set_hover
  =
  let interval_error =
    match Int.of_string_opt (String.strip interval) with
    | Some seconds when seconds >= 10 && seconds <= 3600 -> None
    | Some (_ : int) | None -> Some "interval must be 10 to 3600 seconds"
  in
  let budget_error =
    match arm with
    | false -> None
    | true ->
      (match Float.of_string_opt (String.strip budget) with
       | Some dollars when Float.O.(dollars > 0.) -> None
       | Some (_ : float) | None ->
         Some "budget must be a positive dollar amount")
  in
  let fields_valid =
    Option.is_none interval_error && Option.is_none budget_error
  in
  let labeled label node =
    Vdom.Node.div
      [ Vdom.Node.label
          ~attrs:[ cls "control-label" ]
          [ Vdom.Node.text label ]
      ; node
      ]
  in
  let body =
    match live_run with
    | None ->
      Vdom.Node.div
        ~attrs:[ cls "status" ]
        [ Vdom.Node.text "checking the runner..." ]
    | Some (Error error) -> error_banner ~retry:refresh error
    | Some
        (Ok
          { running
          ; mode
          ; interval_s
          ; ticks
          ; last_tick_s = (_ : float option)
          ; last_summary
          ; spent_dollars
          }) ->
      (match running with
       | true ->
         let mode_line =
           match mode with
           | Paper ->
             [%string "PAPER · scanning every %{interval_s#Int}s"]
           | Armed { budget_dollars } ->
             [%string
               "ARMED · every %{interval_s#Int}s · $%{sprintf \"%.2f\" \
                spent_dollars} of $%{sprintf \"%.2f\" budget_dollars} \
                budget spent"]
         in
         Vdom.Node.div
           [ Vdom.Node.div
               ~attrs:[ cls "arb-summary" ]
               [ Vdom.Node.text
                   [%string
                     "%{mode_line} · %{ticks#Int} tick(s) · last: \
                      %{last_summary}"]
               ]
           ; Vdom.Node.div
               ~attrs:[ cls "button-row" ]
               [ button ~class_:"btn-danger" ~label:"Stop the runner" stop
               ; button ~class_:"btn-secondary" ~label:"Refresh" refresh
               ]
           ]
       | false ->
         Vdom.Node.div
           ([ Vdom.Node.div
                ~attrs:[ cls "arb-summary" ]
                [ Vdom.Node.text [%string "stopped · last: %{last_summary}"]
                ]
            ; Vdom.Node.div
                ~attrs:[ cls "arb-controls" ]
                [ labeled
                    "interval (seconds)"
                    (num_input
                       ?error:interval_error
                       ~value:interval
                       ~set_value:set_interval
                       ())
                ; labeled
                    "arm real trading"
                    (Vdom.Node.input
                       ~attrs:
                         [ cls "checkbox"
                         ; Vdom.Attr.type_ "checkbox"
                         ; Vdom.Attr.bool_property "checked" arm
                         ; on_click (set_arm (not arm))
                         ]
                       ())
                ]
            ]
            @ (match arm with
               | false -> []
               | true ->
                 [ Vdom.Node.div
                     ~attrs:[ cls "arb-controls" ]
                     [ labeled
                         "run budget ($ notional)"
                         (num_input
                            ?error:budget_error
                            ~value:budget
                            ~set_value:set_budget
                            ())
                     ]
                 ; Vdom.Node.p
                     ~attrs:[ cls "arb-disclaimer" ]
                     [ Vdom.Node.text
                         "⚠ Armed mode auto-fires REAL Kalshi orders — the \
                          Kalshi leg only, once per pair per run, inside \
                          the spending caps and kill switch, until this \
                          budget of notional is spent. The position is \
                          ONE-SIDED until you take the Polymarket leg \
                          yourself: an arb signal traded this way is a \
                          directional bet, not a locked-in arb. Requires \
                          your unlocked key; the Wallet page's emergency \
                          stop halts it instantly."
                     ]
                 ])
            @ [ Vdom.Node.div
                  ~attrs:[ cls "button-row" ]
                  [ button
                      ~enabled:fields_valid
                      ~class_:(if arm then "btn-danger" else "btn-primary")
                      ~label:
                        (if arm
                         then "Start ARMED (spends real money)"
                         else "Start paper runner")
                      start
                  ; button ~class_:"btn-secondary" ~label:"Refresh" refresh
                  ]
              ])
       )
  in
  let history_chart =
    match history with
    | None | Some (Error (_ : Error.t)) -> Vdom.Node.none
    | Some (Ok points) ->
      (match List.length points >= 2 with
       | false ->
         Vdom.Node.div
           ~attrs:[ cls "status" ]
           [ Vdom.Node.text
               "not enough sighting history to chart yet — scans (manual \
                or the runner) accrue it"
           ]
       | true ->
         chart_view
           ~title:"strategy paper PnL over time (dollars, from real scans)"
           ~series:[ solid ~name:"locked-in if taken" ~index:0 points ]
           ~hover
           ~set_hover
           ())
  in
  Vdom.Node.div
    ~attrs:[ cls "arb-wallet" ]
    [ Vdom.Node.div
        ~attrs:[ cls "list-heading" ]
        [ Vdom.Node.text "strategy runner" ]
    ; body
    ; history_chart
    ]
;;

let arb_detect_panel
  ~scan_state
  ~scan
  ~wallet
  ~mark_acted
  ~assist
  ~dialog
  ~live_runner
  =
  let running = Request_status.is_running scan_state in
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
    | Failed error -> error_banner ~retry:scan error
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
    ; live_runner
    ; dialog
    ; button
        ~enabled:(not running)
        ~class_:"btn-primary"
        ~label:"Scan approved pairs"
        scan
    ; body
    ]
;;

(* The strategy's own backtest: replay every approved pair's two-venue
   price history and chart what taking each edge once would have locked
   in. The explainer states the model's bias out loud — mid prices, no
   order books — because the numbers WILL flatter the strategy. *)
let arb_backtest_panel
  ~sim_state
  ~lookback
  ~set_lookback
  ~interval
  ~set_interval
  ~stake
  ~set_stake
  ~min_edge
  ~set_min_edge
  ~run
  ~hover
  ~set_hover
  =
  let running = Request_status.is_running sim_state in
  let lookback_error =
    Result.error (Client_logic.Form_validate.lookback_days lookback)
  in
  let stake_error =
    Result.error (Client_logic.Form_validate.arb_stake stake)
  in
  let min_edge_error =
    Result.error (Client_logic.Form_validate.min_edge_cents min_edge)
  in
  let fields_valid =
    List.for_all
      [ lookback_error; stake_error; min_edge_error ]
      ~f:Option.is_none
  in
  let labeled label node =
    Vdom.Node.div
      [ Vdom.Node.label
          ~attrs:[ cls "control-label" ]
          [ Vdom.Node.text label ]
      ; node
      ]
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
  let controls =
    Vdom.Node.div
      ~attrs:[ cls "arb-controls" ]
      [ labeled
          "lookback (days)"
          (num_input
             ?error:lookback_error
             ~value:lookback
             ~set_value:set_lookback
             ())
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
          "stake (contracts)"
          (num_input ?error:stake_error ~value:stake ~set_value:set_stake ())
      ; labeled
          "min edge (cents)"
          (num_input
             ?error:min_edge_error
             ~value:min_edge
             ~set_value:set_min_edge
             ())
      ; button
          ~enabled:((not running) && fields_valid)
          ~class_:"btn-primary"
          ~label:"Run backtest"
          run
      ]
  in
  let explainer =
    Vdom.Node.div
      ~attrs:[ cls "arb-explainer" ]
      [ Vdom.Node.p
          [ Vdom.Node.text
              "Would the strategy have paid? This replays every approved \
               pair's price history on both venues, re-prices the spread \
               each tick with real fees, and takes each edge once the \
               moment it clears your threshold — locking in edge x stake, \
               the way an arb pays whichever way the world goes."
          ]
      ; Vdom.Node.p
          ~attrs:[ cls "arb-disclaimer" ]
          [ Vdom.Node.text
              "⚠ Flattering by construction: history keeps mid prices, \
               not order books, so this replay pays no bid/ask spread and \
               assumes your stake always had depth. Treat results as an \
               upper bound on the live scan, never a promise."
          ]
      ]
  in
  let body =
    match
      (sim_state : Protocol.Arb_sim_result.t Request_status.t)
    with
    | Idle -> Vdom.Node.div []
    | Running ->
      Vdom.Node.div
        ~attrs:[ cls "arb-summary" ]
        [ Vdom.Node.text
            "fetching both legs' history for every approved pair — the \
             venues are polled a second apart, so allow a few seconds per \
             pair..."
        ]
    | Failed error -> error_banner ~retry:run error
    | Done { pairs; combined; total_dollars; episode_count; skipped } ->
      let window_start =
        List.filter_map pairs ~f:(fun pair ->
          List.hd pair.Protocol.Arb_pair_series.points |> Option.map ~f:fst)
        |> List.min_elt ~compare:Float.compare
      in
      (* Cumulative series are steps at entry times; anchor each at zero
         at the window start so a single-entry series still draws. *)
      let anchored points =
        match window_start with
        | Some start -> (start, 0.) :: points
        | None -> points
      in
      let cumulative_chart =
        chart_view
          ~title:"cumulative locked-in profit (dollars)"
          ~series:
            (solid ~name:"all pairs" ~index:0 (anchored combined)
             :: List.mapi pairs ~f:(fun i pair ->
               solid
                 ~name:pair.Protocol.Arb_pair_series.label
                 ~index:(i + 1)
                 (anchored pair.cumulative)))
          ~hover
          ~set_hover
          ()
      in
      let edge_chart =
        chart_view
          ~title:"edge after fees (dollars per contract)"
          ~series:
            (List.mapi pairs ~f:(fun i pair ->
               solid
                 ~name:pair.Protocol.Arb_pair_series.label
                 ~index:i
                 (Client_logic.Downsample.downsample
                    pair.points
                    ~max_points:800)))
          ~hover
          ~set_hover
          ()
      in
      let stats =
        Vdom.Node.div
          ~attrs:[ cls "stats-grid" ]
          [ stat_tile
              ~label:"locked-in profit"
              ~value:(sprintf "$%.2f" total_dollars)
              ~value_class:"stat-pos"
              ()
          ; stat_tile
              ~label:"edges taken"
              ~value:(Int.to_string episode_count)
              ()
          ; stat_tile
              ~label:"pairs replayed"
              ~value:(Int.to_string (List.length pairs))
              ()
          ; stat_tile
              ~label:"pairs skipped"
              ~value:(Int.to_string (List.length skipped))
              ()
          ]
      in
      let pair_row (pair : Protocol.Arb_pair_series.t) =
        Vdom.Node.tr
          [ Vdom.Node.td [ Vdom.Node.text pair.label ]
          ; Vdom.Node.td
              [ Vdom.Node.text
                  (Int.to_string (List.length pair.episodes))
              ]
          ; Vdom.Node.td
              [ Vdom.Node.text (sprintf "$%.2f" pair.total_dollars) ]
          ]
      in
      let header text = Vdom.Node.th [ Vdom.Node.text text ] in
      let pair_table =
        match pairs with
        | [] ->
          Vdom.Node.div
            ~attrs:[ cls "status" ]
            [ Vdom.Node.text "no pair could be replayed" ]
        | pairs ->
          Vdom.Node.div
            ~attrs:[ cls "fills-table-wrap" ]
            [ Vdom.Node.table
                ~attrs:[ cls "fills-table" ]
                (Vdom.Node.tr
                   [ header "pair"; header "edges taken"; header "locked in" ]
                 :: List.map pairs ~f:pair_row)
            ]
      in
      let skipped_rows =
        List.map skipped ~f:(fun (label, reason) ->
          Vdom.Node.div
            ~attrs:[ cls "status" ]
            [ Vdom.Node.text [%string "skipped %{label}: %{reason}"] ])
      in
      Vdom.Node.div
        ([ stats
         ; Vdom.Node.div
             ~attrs:[ cls "charts-grid" ]
             [ cumulative_chart; edge_chart ]
         ; pair_table
         ]
         @ skipped_rows)
  in
  Vdom.Node.div
    ~attrs:[ cls "arb-panel" ]
    [ Vdom.Node.h2
        ~attrs:[ cls "arb-panel-title" ]
        [ Vdom.Node.text "4 · Backtest" ]
    ; Vdom.Node.p
        ~attrs:[ cls "arb-panel-hint" ]
        [ Vdom.Node.text
            "The same PnL treatment the bots get, for the arbitrage \
             strategy: replay the approved pairs and see what disciplined \
             edge-taking would have earned."
        ]
    ; explainer
    ; controls
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
  let sweep_state, set_sweep_state = Bonsai.state Request_status.Idle graph in
  let scan_state, set_scan_state = Bonsai.state Request_status.Idle graph in
  let llm_state, set_llm_state = Bonsai.state Request_status.Idle graph in
  let auto_state, set_auto_state = Bonsai.state Request_status.Idle graph in
  let threshold, set_threshold = Bonsai.state "0.35" graph in
  (* Failures of one-shot actions (decide, mark-acted) land here and render
     as a dismissible banner above the panels. *)
  let action_error, set_action_error = Bonsai.state_opt graph in
  let auto_threshold, set_auto_threshold = Bonsai.state "0.50" graph in
  let api_key, set_api_key = Bonsai.state "" graph in
  (* Backtest state: config strings, the result lifecycle, and one hover
     cell for its charts. *)
  let arb_sim_state, set_arb_sim_state =
    Bonsai.state Request_status.Idle graph
  in
  let sim_lookback, set_sim_lookback = Bonsai.state "14" graph in
  let sim_interval, set_sim_interval =
    Bonsai.state Protocol.Interval.Hour graph
  in
  let sim_stake, set_sim_stake = Bonsai.state "10" graph in
  let sim_min_edge, set_sim_min_edge = Bonsai.state "1" graph in
  let sim_hover, set_sim_hover = Bonsai.state_opt graph in
  (* The live strategy runner's status, its start form, and the forward
     paper-PnL history drawn from recorded sightings. *)
  let live_run, set_live_run = Bonsai.state_opt graph in
  let run_interval, set_run_interval = Bonsai.state "30" graph in
  let arm_real, set_arm_real = Bonsai.state false graph in
  let arm_budget, set_arm_budget = Bonsai.state "20" graph in
  let arb_history, set_arb_history = Bonsai.state_opt graph in
  let dispatch_pairs = Rpc_effect.Rpc.dispatcher Protocol.get_pairs graph in
  let dispatch_arb_sim =
    Rpc_effect.Rpc.dispatcher Protocol.run_arb_simulation graph
  in
  let dispatch_live_run =
    Rpc_effect.Rpc.dispatcher Protocol.get_live_run graph
  in
  let dispatch_start_run =
    Rpc_effect.Rpc.dispatcher Protocol.start_live_run graph
  in
  let dispatch_stop_run =
    Rpc_effect.Rpc.dispatcher Protocol.stop_live_run graph
  in
  let dispatch_history =
    Rpc_effect.Rpc.dispatcher Protocol.get_arb_history graph
  in
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
  and action_error
  and set_action_error
  and auto_threshold
  and set_auto_threshold
  and api_key
  and set_api_key
  and arb_sim_state
  and set_arb_sim_state
  and sim_lookback
  and set_sim_lookback
  and sim_interval
  and set_sim_interval
  and sim_stake
  and set_sim_stake
  and sim_min_edge
  and set_sim_min_edge
  and sim_hover
  and set_sim_hover
  and live_run
  and set_live_run
  and run_interval
  and set_run_interval
  and arm_real
  and set_arm_real
  and arm_budget
  and set_arm_budget
  and arb_history
  and set_arb_history
  and dispatch_live_run
  and dispatch_start_run
  and dispatch_stop_run
  and dispatch_history
  and dispatch_arb_sim
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
  let load_live_run =
    let open Effect.Let_syntax in
    let%bind response = dispatch_live_run () in
    set_live_run (Some (Or_error.join response))
  in
  let load_arb_history =
    let open Effect.Let_syntax in
    let%bind response = dispatch_history () in
    set_arb_history (Some (Or_error.join response))
  in
  let start_live_run =
    match
      ( Int.of_string_opt (String.strip run_interval)
      , (match arm_real with
         | false -> Some Protocol.Live_run.Mode.Paper
         | true ->
           (match Float.of_string_opt (String.strip arm_budget) with
            | Some budget_dollars when Float.O.(budget_dollars > 0.) ->
              Some (Armed { budget_dollars })
            | Some (_ : float) | None -> None)) )
    with
    | Some interval_s, Some mode ->
      let open Effect.Let_syntax in
      let%bind response =
        dispatch_start_run { Protocol.Live_run.Start_request.interval_s; mode }
      in
      (match Or_error.join response with
       | Error error -> set_action_error (Some error)
       | Ok status ->
         Effect.Many
           [ set_action_error None; set_live_run (Some (Ok status)) ])
    | None, (_ : Protocol.Live_run.Mode.t option) | Some (_ : int), None ->
      Effect.Ignore
  in
  let stop_live_run =
    let open Effect.Let_syntax in
    let%bind response = dispatch_stop_run () in
    match Or_error.join response with
    | Error error -> set_action_error (Some error)
    | Ok status ->
      Effect.Many [ set_action_error None; set_live_run (Some (Ok status)) ]
  in
  let refresh_live_run = Effect.Many [ load_live_run; load_arb_history ] in
  let run_arb_sim =
    match
      ( Client_logic.Form_validate.lookback_days sim_lookback
      , Client_logic.Form_validate.arb_stake sim_stake
      , Client_logic.Form_validate.min_edge_cents sim_min_edge )
    with
    | Ok lookback_days, Ok stake, Ok min_edge_cents ->
      let open Effect.Let_syntax in
      let%bind () = set_arb_sim_state Request_status.Running in
      let%bind response =
        dispatch_arb_sim
          { Protocol.Arb_sim_request.lookback_days
          ; interval = sim_interval
          ; stake
          ; min_edge_cents
          }
      in
      (match Or_error.join response with
       | Error error -> set_arb_sim_state (Failed error)
       | Ok result -> set_arb_sim_state (Done result))
    | Error (_ : string), (_ : (int, string) Result.t), _
    | Ok (_ : int), Error (_ : string), _
    | Ok (_ : int), Ok (_ : int), Error (_ : string) -> Effect.Ignore
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
             ; load_live_run
             ; load_arb_history
             ; (let open Effect.Let_syntax in
                let%bind response = dispatch_capability () in
                set_hedge_capability (Some (Or_error.join response)))
             ]
         | Sweep | Backtest -> Effect.Ignore)
      ]
  in
  let mark_acted pair_key =
    let open Effect.Let_syntax in
    let%bind response = dispatch_mark pair_key in
    match Or_error.join response with
    | Error error -> set_action_error (Some error)
    | Ok wallet ->
      Effect.Many [ set_action_error None; set_wallet (Some (Ok wallet)) ]
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
            (* A transport/server error is NOT a refusal: the order may
               have been sent before the connection dropped. Render it as
               Failed with the uncertainty spelled out, never as
               "nothing was sent". *)
            set_hedge_dialog
              (Hedge_dialog.Finished
                 { edge
                 ; result =
                     Protocol.Hedge_result.Failed
                       { error =
                           Other "connection lost - outcome unknown"
                       ; detail =
                           [%string
                             "the request errored in flight, so the hedge \
                              may or may not have been placed. Check your \
                              Kalshi account (or the server's trade log) \
                              before retrying: \
                              %{Error.to_string_hum error}"]
                       ; unhedged = count
                       }
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
    let%bind response =
      dispatch_decide { Protocol.Decide_request.tab; index; new_status }
    in
    match Or_error.join response with
    | Error error -> set_action_error (Some error)
    | Ok (_ : Protocol.Pair_card.t) ->
      Effect.Many [ set_action_error None; load tab ]
  in
  let run_llm =
    let open Effect.Let_syntax in
    let api_key =
      match String.strip api_key with "" -> None | key -> Some key
    in
    let%bind () = set_llm_state Request_status.Running in
    let%bind response =
      dispatch_llm { Protocol.Llm_review_request.api_key }
    in
    match Or_error.join response with
    | Error error -> set_llm_state (Failed error)
    | Ok summary ->
      Effect.Many [ set_llm_state (Done summary); set_pairs None; load tab ]
  in
  let run_auto =
    match Float.of_string_opt auto_threshold with
    | None -> Effect.Ignore
    | Some threshold ->
      let open Effect.Let_syntax in
      let%bind () = set_auto_state Request_status.Running in
      let%bind response =
        dispatch_auto { Protocol.Auto_review_request.threshold }
      in
      (match Or_error.join response with
       | Error error -> set_auto_state (Failed error)
       | Ok summary ->
         Effect.Many
           [ set_auto_state (Done summary); set_pairs None; load tab ])
  in
  let run_sweep =
    match Float.of_string_opt threshold with
    | None -> Effect.Ignore
    | Some threshold ->
      let open Effect.Let_syntax in
      let%bind () = set_sweep_state Request_status.Running in
      let%bind response =
        dispatch_sweep { Protocol.Sweep_request.threshold }
      in
      (match Or_error.join response with
       | Error error -> set_sweep_state (Failed error)
       | Ok summary ->
         Effect.Many [ set_sweep_state (Done summary); load tab ])
  in
  let scan =
    let open Effect.Let_syntax in
    let%bind () = set_scan_state Request_status.Running in
    let%bind response = dispatch_scan () in
    match Or_error.join response with
    | Error error -> set_scan_state (Failed error)
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
      let live_runner =
        arb_live_runner_card
          ~live_run
          ~interval:run_interval
          ~set_interval:set_run_interval
          ~arm:arm_real
          ~set_arm:set_arm_real
          ~budget:arm_budget
          ~set_budget:set_arm_budget
          ~start:start_live_run
          ~stop:stop_live_run
          ~refresh:refresh_live_run
          ~history:arb_history
          ~hover:sim_hover
          ~set_hover:set_sim_hover
      in
      arb_detect_panel
        ~scan_state
        ~scan
        ~wallet
        ~mark_acted
        ~assist
        ~dialog
        ~live_runner
    | Backtest ->
      arb_backtest_panel
        ~sim_state:arb_sim_state
        ~lookback:sim_lookback
        ~set_lookback:set_sim_lookback
        ~interval:sim_interval
        ~set_interval:set_sim_interval
        ~stake:sim_stake
        ~set_stake:set_sim_stake
        ~min_edge:sim_min_edge
        ~set_min_edge:set_sim_min_edge
        ~run:run_arb_sim
        ~hover:sim_hover
        ~set_hover:set_sim_hover
  in
  let us_notice =
    Vdom.Node.div
      ~attrs:[ cls "arb-explainer" ]
      [ Vdom.Node.p
          ~attrs:[ cls "arb-disclaimer" ]
          [ Vdom.Node.text
              "Polymarket's app is not available to US persons, so this \
               app never auto-trades Polymarket. From the US you can still \
               scan both venues to see what arbitrage exists, and place \
               the Kalshi leg through this app (connect a key on the \
               Wallet page); any Polymarket leg is yours to take or skip, \
               on your own eligibility."
          ]
      ]
  in
  let action_banner =
    match action_error with
    | None -> Vdom.Node.none
    | Some error -> error_banner ~dismiss:(set_action_error None) error
  in
  Vdom.Node.div
    [ us_notice
    ; arb_stage_banner ~current:section ~select:select_section
    ; action_banner
    ; panel
    ]
;;

