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
