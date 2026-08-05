(* Shared view building blocks: attribute helpers, the standard
   [button], suggestion lists, market chips, and stat tiles. Meant to
   be [open]ed by the page modules, so the view code that moved here
   from main.ml reads exactly as before the split. *)

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

