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
