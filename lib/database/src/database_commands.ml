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
      category TEXT NOT NULL,
      created_time BIGINT NOT NULL,
      close_time BIGINT NOT NULL
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
    {| INSERT INTO market_stubs (venue, market_id, slug, series_ticker, clob_token_id, title, category, created_time, close_time) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) |}
;;

let find_market_stub =
  let open Caqti_request.Infix in
  (Database_types.market_id_type ->? Database_types.market_stub_type)
    {| SELECT * FROM market_stubs WHERE market_id = ? |}
;;

let list_market_stubs_after =
  let open Caqti_request.Infix in
  (T.t2 T.int64 T.int ->* Database_types.market_stub_type)
    {| SELECT * FROM market_stubs WHERE close_time >= ? LIMIT ? |}
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
