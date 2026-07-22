open! Core
module T = Caqti_type

let create_market_stub_table =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit)
    {|
  CREATE TABLE IF NOT EXISTS market_stubs (
      venue TEXT NOT NULL,
      market_id VARCHAR(255) PRIMARY KEY,
      title TEXT NOT NULL,
      close_time BIGINT
    )
  |}
;;

let insert_market_stub =
  let open Caqti_request.Infix in
  (Database_types.market_stub_type ->. T.unit)
    {| INSERT INTO market_stubs (venue, market_id, title, close_time) VALUES (?, ?, ?, ?) |}
;;

let find_market_stub =
  let open Caqti_request.Infix in
  (Database_types.market_id_type ->? Database_types.market_stub_type)
         {| SELECT * FROM market_stubs WHERE market_id = ? |}
;;
