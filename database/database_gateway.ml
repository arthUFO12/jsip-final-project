open! Core
open! Async
open! Caqti_async
open! Caqti_type

let global_pool = ref None


let init_database () =
  let db_uri = Uri.of_string "sqlite3:///home/ubuntu/jsip-final-project/data.db" in
  match Caqti_async.connect_pool db_uri with
  | Error err -> Error err
  | Ok pool ->
    global_pool := Some pool;
    Ok ()
  ;;