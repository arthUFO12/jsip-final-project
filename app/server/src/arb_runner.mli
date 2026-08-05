open! Core
open! Async

(** The server half of the arbitrage pipeline: one function per {!Protocol}
    RPC, each delegating to the same {!Arbitrage} modules the CLI verbs use,
    so the browser and the terminal always describe the same store. All wire
    conversion (status mirroring, prices as float dollars) happens here.

    Requires {!Database.Database_exec.init_database} and both table creates
    to have run — [server.ml]'s startup does this. *)

(** Backs {!Protocol.get_pairs}. *)
val list_pairs
  :  Protocol.Pair_status.t
  -> Protocol.Pair_card.t list Deferred.Or_error.t

(** Backs {!Protocol.get_pair_counts}. *)
val pair_counts : unit -> Protocol.Pair_counts.t Deferred.Or_error.t

(** Backs {!Protocol.decide_pair}. *)
val decide
  :  Protocol.Decide_request.t
  -> Protocol.Pair_card.t Deferred.Or_error.t

(** Backs {!Protocol.auto_review_pairs}:
    {!Arbitrage.Review.auto_review_proposed} — the deterministic threshold
    gate. *)
val auto_review
  :  Protocol.Auto_review_request.t
  -> Protocol.Auto_review_summary.t Deferred.Or_error.t

(** Backs {!Protocol.llm_review_pairs}:
    {!Arbitrage.Review.llm_review_proposed} over the whole Proposed queue, on
    the caller's key when one is sent. *)
val llm_review
  :  Protocol.Llm_review_request.t
  -> Protocol.Llm_review_summary.t Deferred.Or_error.t

(** Backs {!Protocol.run_sweep}: a full-listing text-only
    {!Arbitrage.Sweep.sweep_full} — never the LLM. *)
val sweep
  :  Protocol.Sweep_request.t
  -> Protocol.Sweep_summary.t Deferred.Or_error.t

(** Backs {!Protocol.scan_edges}: one detection tick over the approved pairs,
    pricing each against both venues' live books without placing orders.
    Tradable edges are booked into the "for fun" wallet as a side effect. *)
val scan : unit -> Protocol.Scan_report.t Deferred.Or_error.t

(** Installs the server's one live executor. Called at startup by [server.ml]
    iff [-allow-live] was passed and Kalshi credentials loaded, and by the
    trading-key unlock path under the same flag; never on a paper server. *)
val enable_live : Execution.Kalshi_live.Credentials.t -> unit

(** Records whether the operator launched with [-allow-live]. Called once at
    startup by [server.ml]; {!unlock_trading_key} refuses without it, so a
    browser can never talk a paper server into going live. *)
val set_live_allowed : bool -> unit

(** Override the spending caps from server flags, validated as a live
    config — zero or negative caps are refused so a rail cannot be
    disabled by flag typo. Call once at startup, before serving. *)
val set_execution_caps
  :  max_order_dollars:float
  -> max_day_dollars:float
  -> unit Or_error.t

(** Backs {!Protocol.get_execution_capability}. *)
val capability : unit -> Protocol.Execution_capability.t Deferred.Or_error.t

(** Backs {!Protocol.get_trading_key}: what the encrypted wallet file and the
    in-memory executor say right now. Never carries secrets. *)
val trading_key_status : unit -> Protocol.Trading_key.Status.t Deferred.Or_error.t

(** Backs {!Protocol.connect_trading_key}: validate the pasted key, encrypt
    it at rest ({!Execution.Wallet_store.save}), and — on a [-allow-live]
    server — unlock it immediately. *)
val connect_trading_key
  :  Protocol.Trading_key.Connect_request.t
  -> Protocol.Trading_key.Status.t Deferred.Or_error.t

(** Backs {!Protocol.unlock_trading_key}: decrypt with the passphrase and
    install the live executor. Refuses without [-allow-live]. *)
val unlock_trading_key
  :  string
  -> Protocol.Trading_key.Status.t Deferred.Or_error.t

(** Backs {!Protocol.lock_trading_key}: drop the executor and decrypted
    credentials from memory; the encrypted file stays. *)
val lock_trading_key : unit -> Protocol.Trading_key.Status.t Deferred.Or_error.t

(** Backs {!Protocol.forget_trading_key}: lock, then delete the file. *)
val forget_trading_key
  :  unit
  -> Protocol.Trading_key.Status.t Deferred.Or_error.t

(** Backs {!Protocol.preflight_hedge}: the advisory rails numbers for
    assisted execution's step 1. *)
val preflight
  :  Protocol.Preflight_request.t
  -> Protocol.Preflight.t Deferred.Or_error.t

(** Backs {!Protocol.execute_hedge}: rebuild the Kalshi hedge order from the
    pair's stored stubs, run the rails, place it sized to the confirmed
    manual count, cancel any resting remainder so [unhedged] is definitive,
    audit-log both legs, and bank the realized dollars into the wallet
    (visibly self-reported). *)
val hedge
  :  Protocol.Hedge_request.t
  -> Protocol.Hedge_result.t Deferred.Or_error.t

(** Backs {!Protocol.get_wallet}: the paper-vs-acted score and its entries. *)
(** The unlocked key's cash and open positions from the venue; errors when
    no key is unlocked. *)
val account : unit -> Protocol.Account.t Deferred.Or_error.t

(** Engage the kill switch with a reason (browser-initiated, one-way);
    returns the engaged reason as confirmation. *)
val trip_kill_switch : string -> string Deferred.Or_error.t

val wallet : unit -> Protocol.Wallet.t Deferred.Or_error.t

(** Backs {!Protocol.mark_acted}: freeze one booked pair as really traded and
    return the refreshed wallet. *)
val mark_acted : string -> Protocol.Wallet.t Deferred.Or_error.t
