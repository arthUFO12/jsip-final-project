(* The Wallet page: connect, unlock, lock, or forget the encrypted Kalshi
   trading key. *)

open! Core
open! Types
open Bonsai_web
open Bonsai.Let_syntax
open Ui

(* ---------- Wallet page: connect / unlock the trading key ---------- *)

(* One trading-key RPC in flight at a time; success lands in the shared
   status card, so [Done] never carries a payload. *)
module Key_op = struct
  type t = unit Request_status.t
end

let wallet_disclaimer =
  let p text = Vdom.Node.p [ Vdom.Node.text text ] in
  Vdom.Node.div
    ~attrs:[ cls "wallet-disclaimer" ]
    [ p
        "Polymarket does not serve US persons, so this app never places \
         Polymarket orders for you and has no Polymarket key to connect — \
         no auto-trading on that venue, period. What you can do from the \
         US: scan both venues to see what arbitrage exists, and place the \
         Kalshi side of a trade through this app with the key below."
    ; p
        "Connecting a key lets this app place real orders on your Kalshi \
         account, spending real money, entirely at your own risk. Nothing \
         here is financial advice. Start with a demo key: demo trades are \
         free and use the same code path."
    ; p
        "Your key never leaves this machine except to sign requests to \
         Kalshi. It is encrypted with your passphrase (AES-256-GCM, key \
         derived by PBKDF2) and stored only in ~/.kalshi/arbiter-wallet on \
         the computer running the server. We cannot recover a lost \
         passphrase — forget the key and paste it again."
    ; p
        "Even with a key unlocked, orders only happen on a server started \
         with -allow-live, inside the spending caps, and never without an \
         explicit confirmation click."
    ]
;;

let wallet_page (local_ graph) =
  let status, set_status = Bonsai.state_opt graph in
  let op, set_op = Bonsai.state Request_status.Idle graph in
  let key_id, set_key_id = Bonsai.state "" graph in
  let pem, set_pem = Bonsai.state "" graph in
  let passphrase, set_passphrase = Bonsai.state "" graph in
  let passphrase2, set_passphrase2 = Bonsai.state "" graph in
  let production, set_production = Bonsai.state false graph in
  let unlock_passphrase, set_unlock_passphrase = Bonsai.state "" graph in
  (* The venue account (cash + positions) behind the unlocked key, and the
     engaged reason after tripping the kill switch from this page. *)
  let account, set_account = Bonsai.state_opt graph in
  let tripped, set_tripped = Bonsai.state_opt graph in
  let dispatch_get =
    Rpc_effect.Rpc.dispatcher Protocol.get_trading_key graph
  in
  let dispatch_account =
    Rpc_effect.Rpc.dispatcher Protocol.get_account graph
  in
  let dispatch_trip =
    Rpc_effect.Rpc.dispatcher Protocol.trip_kill_switch graph
  in
  let dispatch_connect =
    Rpc_effect.Rpc.dispatcher Protocol.connect_trading_key graph
  in
  let dispatch_unlock =
    Rpc_effect.Rpc.dispatcher Protocol.unlock_trading_key graph
  in
  let dispatch_lock =
    Rpc_effect.Rpc.dispatcher Protocol.lock_trading_key graph
  in
  let dispatch_forget =
    Rpc_effect.Rpc.dispatcher Protocol.forget_trading_key graph
  in
  let on_activate =
    let%map dispatch_get
    and set_status
    and dispatch_account
    and set_account in
    let%bind.Effect response = dispatch_get () in
    let%bind.Effect () = set_status (Some (Or_error.join response)) in
    (* Errors here just mean "locked"; the card only renders when the key
       status says unlocked. *)
    let%bind.Effect account = dispatch_account () in
    set_account (Some (Or_error.join account))
  in
  Bonsai.Edge.lifecycle ~on_activate graph;
  let%arr status
  and set_status
  and op
  and set_op
  and key_id
  and set_key_id
  and pem
  and set_pem
  and passphrase
  and set_passphrase
  and passphrase2
  and set_passphrase2
  and production
  and set_production
  and unlock_passphrase
  and set_unlock_passphrase
  and account
  and set_account
  and tripped
  and set_tripped
  and dispatch_connect
  and dispatch_unlock
  and dispatch_lock
  and dispatch_forget
  and dispatch_account
  and dispatch_trip in
  let running = Request_status.is_running op in
  (* Every mutation follows the same shape: mark busy, dispatch, land the
     fresh status (clearing any secrets the form was holding) or keep the
     error. *)
  let run_op ?(on_ok = Effect.Ignore) dispatch =
    let open Effect.Let_syntax in
    let%bind () = set_op Request_status.Running in
    let%bind response = dispatch in
    match Or_error.join response with
    | Ok new_status ->
      Effect.Many
        [ set_status (Some (Ok new_status))
        ; set_op Request_status.Idle
        ; on_ok
        ]
    | Error error -> set_op (Failed error)
  in
  let load_account =
    let open Effect.Let_syntax in
    let%bind response = dispatch_account () in
    set_account (Some (Or_error.join response))
  in
  let clear_secrets =
    Effect.Many
      [ set_pem ""
      ; set_passphrase ""
      ; set_passphrase2 ""
      ; set_unlock_passphrase ""
      ]
  in
  let labeled label node =
    Vdom.Node.div
      ~attrs:[ cls "wallet-row" ]
      [ Vdom.Node.label
          ~attrs:[ cls "wallet-label" ]
          [ Vdom.Node.text label ]
      ; node
      ]
  in
  let text_input ~password ~placeholder value set =
    Vdom.Node.input
      ~attrs:
        [ cls "arb-key-input"
        ; Vdom.Attr.type_ (if password then "password" else "text")
        ; Vdom.Attr.placeholder placeholder
        ; Vdom.Attr.value value
        ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text -> set text)
        ]
      ()
  in
  let op_status =
    match (op : Key_op.t) with
    | Idle | Done () -> Vdom.Node.none
    | Running ->
      Vdom.Node.div
        ~attrs:[ cls "status" ]
        [ Vdom.Node.text "talking to the server..." ]
    | Failed error -> error_box error
  in
  let connect_form =
    let passphrases_match = String.equal passphrase passphrase2 in
    let passphrase_long_enough = String.length passphrase >= 8 in
    let form_complete =
      (not (String.is_empty (String.strip key_id)))
      && String.is_substring pem ~substring:"PRIVATE KEY"
      && passphrase_long_enough
      && passphrases_match
    in
    let connect =
      run_op
        ~on_ok:(Effect.Many [ clear_secrets; set_key_id "" ])
        (dispatch_connect
           { Protocol.Trading_key.Connect_request.key_id =
               String.strip key_id
           ; private_key_pem = pem
           ; passphrase
           ; production
           })
    in
    Vdom.Node.div
      ~attrs:[ cls "wallet-form" ]
      [ labeled
          "Kalshi API key ID (from kalshi.com account settings)"
          (text_input
             ~password:false
             ~placeholder:"e.g. cc2a8db2-048d-44c1-9105-..."
             key_id
             set_key_id)
      ; labeled
          "RSA private key (paste the whole .pem file Kalshi gave you)"
          (Vdom.Node.textarea
             ~attrs:
               [ cls "wallet-pem"
               ; Vdom.Attr.placeholder "-----BEGIN PRIVATE KEY-----\n..."
               ; Vdom.Attr.value pem
               ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
                   set_pem text)
               ]
             [])
      ; labeled
          "passphrase (encrypts the key on disk; at least 8 characters)"
          (text_input
             ~password:true
             ~placeholder:"choose a passphrase"
             passphrase
             set_passphrase)
      ; labeled
          "passphrase again"
          (text_input
             ~password:true
             ~placeholder:"same passphrase"
             passphrase2
             set_passphrase2)
      ; (match passphrases_match || String.is_empty passphrase2 with
         | true -> Vdom.Node.none
         | false ->
           Vdom.Node.div
             ~attrs:[ cls "wallet-mismatch" ]
             [ Vdom.Node.text "passphrases do not match" ])
      ; Vdom.Node.div
          ~attrs:[ cls "chip-row" ]
          [ Vdom.Node.input
              ~attrs:
                [ cls "checkbox"
                ; Vdom.Attr.type_ "checkbox"
                ; Vdom.Attr.bool_property "checked" production
                ; on_click (set_production (not production))
                ]
              ()
          ; Vdom.Node.label
              ~attrs:[ cls "wallet-label" ]
              [ Vdom.Node.text
                  "this is a PRODUCTION key (real money), not a demo key"
              ]
          ]
      ; (match production with
         | false -> Vdom.Node.none
         | true ->
           Vdom.Node.div
             ~attrs:[ cls "wallet-prod-warning" ]
             [ Vdom.Node.text
                 "Production means real dollars leave your account when a \
                  trade goes through. Make sure the spending caps in your \
                  config are set the way you want before unlocking."
             ])
      ; Vdom.Node.div
          ~attrs:[ cls "button-row" ]
          [ button
              ~enabled:(form_complete && not running)
              ~class_:"btn-primary"
              ~label:"Encrypt and connect"
              connect
          ]
      ]
  in
  let status_card
    { Protocol.Trading_key.Status.connected = (_ : bool)
    ; unlocked
    ; key_hint
    ; production
    ; live_allowed
    }
    =
    let badge =
      match production with
      | true ->
        Vdom.Node.span
          ~attrs:[ cls "wallet-badge wallet-badge-prod" ]
          [ Vdom.Node.text "production - real money" ]
      | false ->
        Vdom.Node.span
          ~attrs:[ cls "wallet-badge wallet-badge-demo" ]
          [ Vdom.Node.text "demo - pretend money" ]
    in
    let state_line =
      match unlocked, live_allowed with
      | true, (_ : bool) ->
        Vdom.Node.div
          ~attrs:[ cls "wallet-state-line wallet-unlocked" ]
          [ Vdom.Node.text
              "Unlocked - this server can sign orders with your key until \
               it restarts or you lock it."
          ]
      | false, true ->
        Vdom.Node.div
          ~attrs:[ cls "wallet-state-line wallet-locked" ]
          [ Vdom.Node.text
              "Locked - enter your passphrase to unlock trading for this \
               server session."
          ]
      | false, false ->
        Vdom.Node.div
          ~attrs:[ cls "wallet-state-line wallet-locked" ]
          [ Vdom.Node.text
              "Locked - and this server was started without -allow-live, so \
               it cannot trade at all. Restart it with the flag to unlock."
          ]
    in
    let unlock_row =
      match unlocked with
      | true ->
        Vdom.Node.div
          ~attrs:[ cls "button-row" ]
          [ button
              ~enabled:(not running)
              ~class_:"btn-secondary"
              ~label:"Lock"
              (run_op ~on_ok:(set_account None) (dispatch_lock ()))
          ]
      | false ->
        Vdom.Node.div
          ~attrs:[ cls "arb-controls" ]
          [ text_input
              ~password:true
              ~placeholder:"passphrase"
              unlock_passphrase
              set_unlock_passphrase
          ; button
              ~enabled:
                ((not (String.is_empty unlock_passphrase))
                 && (not running)
                 && live_allowed)
              ~class_:"btn-primary"
              ~label:"Unlock"
              (run_op
                 ~on_ok:
                   (Effect.Many [ set_unlock_passphrase ""; load_account ])
                 (dispatch_unlock unlock_passphrase))
          ]
    in
    Vdom.Node.div
      ~attrs:[ cls "wallet-status-card" ]
      [ Vdom.Node.div
          [ Vdom.Node.span
              ~attrs:[ cls "wallet-key-hint" ]
              [ Vdom.Node.text
                  (Option.value key_hint ~default:"(key on file)")
              ]
          ; badge
          ]
      ; state_line
      ; unlock_row
      ; Vdom.Node.div
          ~attrs:[ cls "button-row" ]
          [ button
              ~enabled:(not running)
              ~class_:"btn-secondary"
              ~label:"Forget this key (deletes the encrypted file)"
              (run_op
                 ~on_ok:(Effect.Many [ clear_secrets; set_account None ])
                 (dispatch_forget ()))
          ]
      ]
  in
  (* What the venue says the unlocked key holds — the ground truth the trade
     log and wallet scoreboard approximate. *)
  let account_card =
    let position_row
      ({ ticker; position; exposure_dollars } : Protocol.Account.Position.t)
      =
      Vdom.Node.div
        ~attrs:[ cls "arb-wallet-entry" ]
        [ Vdom.Node.span
            ~attrs:[ cls "wallet-key-hint" ]
            [ Vdom.Node.text ticker ]
        ; Vdom.Node.span
            [ Vdom.Node.text (sprintf "%+d contracts" position) ]
        ; Vdom.Node.span
            ~attrs:[ cls "arb-wallet-entry-dollars" ]
            [ Vdom.Node.text [%string "%{money exposure_dollars} exposure"] ]
        ]
    in
    let body =
      match account with
      | None ->
        Vdom.Node.div
          ~attrs:[ cls "status" ]
          [ Vdom.Node.text "loading the venue account..." ]
      | Some (Error error) -> error_banner ~retry:load_account error
      | Some (Ok { Protocol.Account.balance_dollars; positions; production })
        ->
        let badge =
          match production with
          | true ->
            Vdom.Node.span
              ~attrs:[ cls "wallet-badge wallet-badge-prod" ]
              [ Vdom.Node.text "production - real money" ]
          | false ->
            Vdom.Node.span
              ~attrs:[ cls "wallet-badge wallet-badge-demo" ]
              [ Vdom.Node.text "demo - pretend money" ]
        in
        Vdom.Node.div
          ([ Vdom.Node.div
               [ Vdom.Node.span
                   ~attrs:[ cls "arb-wallet-amount arb-wallet-real" ]
                   [ Vdom.Node.text (money balance_dollars) ]
               ; badge
               ]
           ; Vdom.Node.div
               ~attrs:[ cls "wallet-hint" ]
               [ Vdom.Node.text "available cash on the venue" ]
           ]
           @ (match positions with
              | [] ->
                [ Vdom.Node.div
                    ~attrs:[ cls "status" ]
                    [ Vdom.Node.text "no open positions" ]
                ]
              | positions -> List.map positions ~f:position_row)
           @ [ Vdom.Node.div
                 ~attrs:[ cls "button-row" ]
                 [ button
                     ~class_:"btn-secondary"
                     ~label:"Refresh"
                     load_account
                 ]
             ])
    in
    Vdom.Node.div
      ~attrs:[ cls "wallet-status-card" ]
      [ Vdom.Node.div
          ~attrs:[ cls "list-heading" ]
          [ Vdom.Node.text "venue account" ]
      ; body
      ]
  in
  (* One-way from here: the button creates the sentinel file on the server;
     clearing it back requires shell access to that machine. *)
  let emergency_card =
    let trip =
      let open Effect.Let_syntax in
      let%bind response = dispatch_trip "tripped from the wallet page" in
      match Or_error.join response with
      | Ok reason -> set_tripped (Some reason)
      | Error error -> set_op (Failed error)
    in
    let body =
      match tripped with
      | Some reason ->
        Vdom.Node.div
          ~attrs:[ cls "arb-onesided" ]
          [ Vdom.Node.text
              [%string
                "KILL SWITCH ENGAGED (%{reason}) — every live order \
                 now                  refuses. To resume, delete the \
                 trading.disabled file in                  the server's \
                 working directory."]
          ]
      | None ->
        Vdom.Node.div
          [ Vdom.Node.p
              ~attrs:[ cls "wallet-hint" ]
              [ Vdom.Node.text
                  "Stops every live order this server could \
                   place,                    immediately, including \
                   assisted hedges mid-flow.                    One-way \
                   from the browser: resuming requires \
                   deleting                    the sentinel file on the \
                   server machine."
              ]
          ; button ~class_:"btn-danger" ~label:"STOP ALL LIVE TRADING" trip
          ]
    in
    Vdom.Node.div
      ~attrs:[ cls "wallet-status-card" ]
      [ Vdom.Node.div
          ~attrs:[ cls "list-heading" ]
          [ Vdom.Node.text "emergency stop" ]
      ; body
      ]
  in
  let body =
    match status with
    | None ->
      Vdom.Node.div
        ~attrs:[ cls "status" ]
        [ Vdom.Node.text "checking for a connected key..." ]
    | Some (Error error) -> error_box error
    | Some (Ok key_status) ->
      (match key_status.Protocol.Trading_key.Status.connected with
       | true ->
         Vdom.Node.div
           ([ status_card key_status ]
            @ (match key_status.unlocked with
               | true -> [ account_card ]
               | false -> [])
            @
            match key_status.live_allowed with
            | true -> [ emergency_card ]
            | false -> [])
       | false -> connect_form)
  in
  Vdom.Node.div
    [ Vdom.Node.h2 [ Vdom.Node.text "Connect your trading key" ]
    ; wallet_disclaimer
    ; body
    ; op_status
    ]
;;
