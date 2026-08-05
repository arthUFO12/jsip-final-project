open! Core
module T = Caqti_type

let create_market_stub_table =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit)
    {|
  CREATE TABLE IF NOT EXISTS market_stubs (
      venue TEXT NOT NULL,
      market_id VARCHAR(255) PRIMARY KEY,
      slug VARCHAR(255) NOT NULL,
      series_ticker VARCHAR(255),
      clob_token_id TEXT,
      title TEXT NOT NULL,
      created_time BIGINT NOT NULL,
      close_time BIGINT NOT NULL,
      category TEXT NOT NULL,
      volume TEXT
    )
  |}
;;

let create_config_table =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit)
    {|
  CREATE TABLE IF NOT EXISTS configs (
    bot_id INT PRIMARY KEY,
    config_sexp TEXT
  )|}
;;

let insert_market_stub =
  let open Caqti_request.Infix in
  (Database_types.market_stub_type ->. T.unit)
    {| INSERT OR REPLACE INTO market_stubs (venue, market_id, slug, series_ticker, clob_token_id, title, created_time, close_time, category, volume) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |}
;;

let find_market_stub =
  let open Caqti_request.Infix in
  (Database_types.market_id_type ->? Database_types.market_stub_type)
    {| SELECT * FROM market_stubs WHERE market_id = ? |}
;;

let find_market_stub_by_slug =
  let open Caqti_request.Infix in
  (T.string ->? Database_types.market_stub_type)
    {| SELECT * FROM market_stubs WHERE slug = ? LIMIT 1 |}
;;

let list_market_stubs_after =
  let open Caqti_request.Infix in
  (T.t2 T.int64 T.int ->* Database_types.market_stub_type)
    {| SELECT * FROM market_stubs WHERE close_time >= ? LIMIT ? |}
;;

let delete_market_stubs =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit) {| DELETE FROM market_stubs |}
;;

let list_market_stubs_before =
  let open Caqti_request.Infix in
  (T.t2 T.int64 T.int ->* Database_types.market_stub_type)
    {| SELECT * FROM market_stubs WHERE close_time < ? LIMIT ? |}
;;

let count_market_stubs_after =
  let open Caqti_request.Infix in
  (T.int64 ->! T.int)
    {| SELECT COUNT(*) FROM market_stubs WHERE close_time >= ? |}
;;

let count_market_stubs_before =
  let open Caqti_request.Infix in
  (T.int64 ->! T.int)
    {| SELECT COUNT(*) FROM market_stubs WHERE close_time < ? |}
;;

let delete_market_stub =
  let open Caqti_request.Infix in
  (Database_types.market_id_type ->. T.unit)
    {| DELETE FROM market_stubs WHERE market_id = ? |}
;;

let list_oldest_market_stubs_before =
  let open Caqti_request.Infix in
  (T.t2 T.int64 T.int ->* Database_types.market_stub_type)
    {| SELECT * FROM market_stubs WHERE close_time < ? ORDER BY close_time ASC LIMIT ? |}
;;

let create_pair_proposal_table =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit)
    {|
  CREATE TABLE IF NOT EXISTS pair_proposals (
      left_id VARCHAR(255) NOT NULL,
      right_id VARCHAR(255) NOT NULL,
      score FLOAT NOT NULL,
      explanation TEXT,
      status TEXT NOT NULL,
      PRIMARY KEY (left_id, right_id)
    )
  |}
;;

(* DO NOTHING is the review gate's memory: re-proposing a pair a human
   already decided on must not resurrect or reset it. *)
let insert_pair_proposal =
  let open Caqti_request.Infix in
  (Database_types.pair_proposal_type ->. T.unit)
    {| INSERT INTO pair_proposals (left_id, right_id, score, explanation, status)
       VALUES (?, ?, ?, ?, ?)
       ON CONFLICT (left_id, right_id) DO NOTHING |}
;;

let set_pair_status =
  let open Caqti_request.Infix in
  (T.t3
     Database_types.pair_status_type
     Database_types.market_id_type
     Database_types.market_id_type
   ->. T.unit)
    {| UPDATE pair_proposals SET status = ? WHERE left_id = ? AND right_id = ? |}
;;

let list_pair_proposals_by_status =
  let open Caqti_request.Infix in
  (Database_types.pair_status_type ->* Database_types.pair_proposal_type)
    {| SELECT * FROM pair_proposals WHERE status = ? ORDER BY score DESC |}
;;

(* [set_pair_status] plus the adjudicator's rationale in one write — the LLM
   review path, where the explanation is the audit trail. *)
let set_pair_verdict =
  let open Caqti_request.Infix in
  (T.t4
     Database_types.pair_status_type
     (T.option T.string)
     Database_types.market_id_type
     Database_types.market_id_type
   ->. T.unit)
    {| UPDATE pair_proposals SET status = ?, explanation = ?
       WHERE left_id = ? AND right_id = ? |}
;;

(* Same columns as [market_stubs]: the pair store's own snapshot of each leg,
   taken at proposal time. [market_stubs] is a rotating catalog whose seed
   purges rows at will; rows here are only ever replaced by a sweep that sees
   the market again. *)
let create_pair_stub_table =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit)
    {|
  CREATE TABLE IF NOT EXISTS pair_stubs (
      venue TEXT NOT NULL,
      market_id VARCHAR(255) PRIMARY KEY,
      slug VARCHAR(255) NOT NULL,
      series_ticker VARCHAR(255),
      clob_token_id TEXT,
      title TEXT NOT NULL,
      created_time BIGINT NOT NULL,
      close_time BIGINT NOT NULL,
      category TEXT NOT NULL,
      volume TEXT
    )
  |}
;;

let insert_pair_stub =
  let open Caqti_request.Infix in
  (Database_types.market_stub_type ->. T.unit)
    {| INSERT OR REPLACE INTO pair_stubs (venue, market_id, slug, series_ticker, clob_token_id, title, created_time, close_time, category, volume) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |}
;;

let find_pair_stub =
  let open Caqti_request.Infix in
  (Database_types.market_id_type ->? Database_types.market_stub_type)
    {| SELECT * FROM pair_stubs WHERE market_id = ? |}
;;

(* One-time rescue for stores from before [pair_stubs] existed: copy every
   catalog row a proposal references, keeping any snapshot already taken. The
   SELECT * relies on both tables declaring identical columns. *)
let backfill_pair_stubs =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit)
    {| INSERT OR IGNORE INTO pair_stubs
       SELECT * FROM market_stubs
       WHERE market_id IN
         (SELECT left_id FROM pair_proposals
          UNION SELECT right_id FROM pair_proposals) |}
;;

(* The "for fun" paper-trading wallet: one row per pair ever seen tradable.
   [INSERT .. DO UPDATE .. WHERE acted = 0] keeps unacted rows tracking the
   latest observation while frozen rows (the user really traded them) never
   move again. *)
let create_arb_wallet_table =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit)
    {|
  CREATE TABLE IF NOT EXISTS arb_wallet (
      pair_key TEXT PRIMARY KEY,
      summary TEXT NOT NULL,
      edge FLOAT NOT NULL,
      size INT NOT NULL,
      dollars FLOAT NOT NULL,
      acted INT NOT NULL,
      acted_dollars FLOAT NOT NULL
    )
  |}
;;

let upsert_wallet_entry =
  let open Caqti_request.Infix in
  (Database_types.wallet_entry_type ->. T.unit)
    {| INSERT INTO arb_wallet (pair_key, summary, edge, size, dollars, acted, acted_dollars)
       VALUES (?, ?, ?, ?, ?, ?, ?)
       ON CONFLICT (pair_key) DO UPDATE SET
         summary = excluded.summary,
         edge = excluded.edge,
         size = excluded.size,
         dollars = excluded.dollars
       WHERE acted = 0 |}
;;

let mark_wallet_acted =
  let open Caqti_request.Infix in
  (T.string ->. T.unit)
    {| UPDATE arb_wallet SET acted = 1, acted_dollars = dollars
       WHERE pair_key = ? AND acted = 0 |}
;;

let list_wallet_entries =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->* Database_types.wallet_entry_type)
    {| SELECT * FROM arb_wallet ORDER BY dollars DESC |}
;;

(* The live-trading audit trail: append-only by construction — no UPDATE or
   DELETE statement exists for it anywhere in this module. *)
let create_trade_log_table =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit)
    {|
  CREATE TABLE IF NOT EXISTS trade_log (
      at BIGINT NOT NULL,
      venue TEXT NOT NULL,
      market_id TEXT NOT NULL,
      action TEXT NOT NULL,
      client_order_id TEXT,
      outcome TEXT NOT NULL,
      detail TEXT NOT NULL,
      dollars FLOAT NOT NULL
    )
  |}
;;

let append_trade_log =
  let open Caqti_request.Infix in
  (Database_types.trade_log_entry_type ->. T.unit)
    {| INSERT INTO trade_log (at, venue, market_id, action, client_order_id, outcome, detail, dollars)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?) |}
;;

let list_trade_log =
  let open Caqti_request.Infix in
  (T.int ->* Database_types.trade_log_entry_type)
    {| SELECT * FROM trade_log ORDER BY rowid DESC LIMIT ? |}
;;

(* What the per-day live spending cap sums: accepted placements since the
   cutoff. Refusals and cancels are logged with zero dollars anyway, but
   filtering on action keeps the intent explicit. *)
let sum_trade_dollars_since =
  let open Caqti_request.Infix in
  (T.int64 ->! T.float)
    {| SELECT COALESCE(SUM(dollars), 0) FROM trade_log
       WHERE at >= ? AND action = 'place' |}
;;

(* Assisted executions bank real (but partly self-reported) dollars: the
   summary gains a visible marker so the wallet's scoreboard never shows
   say-so money as venue-verified. *)
let mark_wallet_acted_assisted =
  let open Caqti_request.Infix in
  (T.t2 T.float T.string ->. T.unit)
    {| UPDATE arb_wallet
       SET acted = 1, acted_dollars = ?, summary = '[self-reported] ' || summary
       WHERE pair_key = ? AND acted = 0 |}
;;
