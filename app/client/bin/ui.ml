(* Shared view building blocks: attribute helpers, the standard [button],
   suggestion lists, market chips, and stat tiles. Meant to be [open]ed by
   the page modules, so the view code that moved here from main.ml reads
   exactly as before the split. *)

open! Core
open! Types
open! Bonsai_web
module Card = Protocol.Market_card

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

(* Keeps a market-chip name to roughly one row of the rules layout. *)
let max_title_length = 60

let truncate_title title =
  match String.length title > max_title_length with
  | false -> title
  | true -> String.prefix title max_title_length ^ "..."
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

(* Dollar amounts everywhere in the UI: thousands grouped, two decimals, and
   the sign ahead of the currency symbol ([money (-3.5)] is "-$3.50", not
   "$-3.50"). *)
let money value =
  let sign = match Float.(value < 0.) with true -> "-" | false -> "" in
  let magnitude =
    Float.to_string_hum
      (Float.abs value)
      ~decimals:2
      ~delimiter:','
      ~strip_zero:false
  in
  [%string "%{sign}$%{magnitude}"]
;;

(* Returns and their parts are signed quantities, so their value carries the
   direction color; balances (cash, portfolio, total) stay in plain ink. *)
let signed_stat_tile ~label value =
  let value_class =
    match Float.(value < 0.) with true -> "stat-neg" | false -> "stat-pos"
  in
  stat_tile ~label ~value:(money value) ~value_class ()
;;

(* The one lifecycle every background request shares. Pages alias it with
   their payload ([Sweep_summary.t Request_status.t], ...) so a server
   failure always lands in a [Failed] arm the view must render — the old
   per-page state machines had no failure arm and dropped errors on the
   floor. *)
module Request_status = struct
  type 'a t =
    | Idle
    | Running
    | Failed of Error.t
    | Done of 'a
  [@@deriving sexp_of]

  let is_running = function
    | Running -> true
    | Idle | Failed (_ : Error.t) | Done _ -> false
  ;;
end

let error_box error =
  Vdom.Node.div
    ~attrs:[ cls "error-box" ]
    [ Vdom.Node.text (Error.to_string_hum error) ]
;;

(* A failure the user can act on: the message plus an optional retry of the
   request that failed, and an optional dismiss. *)
let error_banner ?retry ?dismiss error =
  let retry_button =
    match retry with
    | None -> []
    | Some retry -> [ button ~class_:"btn-secondary" ~label:"Retry" retry ]
  in
  let dismiss_button =
    match dismiss with
    | None -> []
    | Some dismiss ->
      [ Vdom.Node.button
          ~attrs:[ cls "chip-remove"; on_click dismiss ]
          [ Vdom.Node.text "✕" ]
      ]
  in
  Vdom.Node.div
    ~attrs:[ cls "error-banner" ]
    (Vdom.Node.div
       ~attrs:[ cls "error-banner-text" ]
       [ Vdom.Node.text (Error.to_string_hum error) ]
     :: (retry_button @ dismiss_button))
;;

let field_error = function
  | None -> Vdom.Node.none
  | Some message ->
    Vdom.Node.div ~attrs:[ cls "field-error" ] [ Vdom.Node.text message ]
;;

(* A numeric text field that flags itself and explains when [error] is
   present — silent button-disabling gives the user nothing to fix. *)
let num_input ?error ~value ~set_value () =
  Vdom.Node.div
    [ Vdom.Node.input
        ~attrs:
          [ Vdom.Attr.classes
              ("num-input"
               ::
               (match error with
                | None -> []
                | Some (_ : string) -> [ "input-invalid" ]))
          ; Vdom.Attr.value value
            (* Numeric on-screen keyboard on touch devices; no effect on
               desktop. *)
          ; Vdom.Attr.create "inputmode" "decimal"
          ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
              set_value text)
          ]
        ()
    ; field_error error
    ]
;;
