(* The Markets page: the category-grouped browser, the market detail popup,
   and the shared [fetch_markets] whose result the Bots page reuses. *)

open! Core
open! Types
open Bonsai_web
open Bonsai.Let_syntax
open Ui
open Chart_view
module Card = Protocol.Market_card

let max_per_category = 5
let min_per_category = 3

(* ---------- Markets page ---------- *)

let volume_string (card : Card.t) =
  match card.volume with
  | None -> "volume unreported"
  | Some (Contracts count) ->
    let count = Int.to_string_hum ~delimiter:',' count in
    [%string "%{count} contracts"]
  | Some (Notional_dollars dollars) -> [%string "%{money dollars} traded"]
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
          (* The dot's color comes from the stylesheet's per-category [cat-*]
             tokens. *)
            ~attrs:
              [ Vdom.Attr.classes
                  [ "category-dot"
                  ; [%string
                      "cat-%{String.lowercase (Category.to_string category)}"]
                  ]
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

let markets_page_view markets ~on_select ~refetch =
  match markets with
  | None ->
    Vdom.Node.div
      ~attrs:[ cls "status" ]
      [ Vdom.Node.text "loading markets..." ]
  | Some (Error error) -> error_banner ~retry:refetch error
  | Some (Ok cards) ->
    (* Markets are fetched once per app load, so give the user a way to
       pick up newly listed markets without reloading the whole page. *)
    Vdom.Node.div
      [ markets_view cards ~on_select
      ; Vdom.Node.div
          ~attrs:[ cls "button-row" ]
          [ button ~class_:"btn-secondary" ~label:"Refresh markets" refetch ]
      ]
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

let market_detail_modal
  (card : Card.t)
  (detail : Detail_state.t)
  ~close
  ~hover
  ~set_hover
  =
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
              ~value:(money price)
              ()
          ]
      in
      let volume_24h_tile =
        match List.last trailing_volumes with
        | None -> []
        | Some (_, volume) ->
          [ stat_tile
              ~label:"24h volume"
              ~value:
                (let count =
                   Float.to_string_hum volume ~decimals:0 ~delimiter:','
                 in
                 [%string "%{count} contracts"])
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
          ~series:[ solid ~name:"yes price" ~index:0 prices ]
          ~hover
          ~set_hover
          ()
      in
      let volume_chart =
        match trailing_volumes with
        | [] -> []
        | _ :: _ ->
          [ chart_view
              ~title:"24h volume (contracts)"
              ~series:[ solid ~name:"24h volume" ~index:1 trailing_volumes ]
              ~hover
              ~set_hover
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

let markets_page markets_result ~refetch (local_ graph) =
  let selected, set_selected = Bonsai.state (None : Card.t option) graph in
  let detail, set_detail = Bonsai.state Detail_state.Idle graph in
  (* One hover cell for the modal's charts, keyed by chart title. *)
  let hover, set_hover = Bonsai.state_opt graph in
  let dispatch_detail =
    Rpc_effect.Rpc.dispatcher Protocol.get_market_detail graph
  in
  let%arr selected
  and set_selected
  and detail
  and set_detail
  and hover
  and set_hover
  and dispatch_detail
  and markets_result
  and refetch in
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
    | Some card -> market_detail_modal card detail ~close ~hover ~set_hover
  in
  Vdom.Node.div
    [ markets_page_view markets_result ~on_select ~refetch; modal ]
;;

(* ---------- Markets data fetch ---------- *)

(* Returns the fetch result alongside the fetch effect itself, so a failed
   initial load can offer Retry instead of demanding a page reload. *)
let fetch_markets (local_ graph) =
  (* [where_to_connect] defaults to the serving host's websocket. *)
  let dispatch = Rpc_effect.Rpc.dispatcher Protocol.get_markets graph in
  let result, set_result = Bonsai.state_opt graph in
  let fetch =
    let%map dispatch and set_result in
    (* Drop back to the loading state first so a retry shows progress
       rather than the stale error while the request is in flight. *)
    let%bind.Effect () = set_result None in
    let%bind.Effect response = dispatch () in
    set_result (Some (Or_error.join response))
  in
  Bonsai.Edge.lifecycle ~on_activate:fetch graph;
  result, fetch
;;
