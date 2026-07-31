open! Core
open! Async
open! Caqti_async
open! Caqti_type
open Types

let global_pool = ref None
let db_folder = "sqlite3:///home/ubuntu/jsip-final-project/"

let init_database db_name =
  let db_uri = Uri.of_string (db_folder ^ db_name) in
  match Caqti_async.connect_pool db_uri with
  | Error err -> Or_error.error_string (Caqti_error.show err)
  | Ok pool ->
    global_pool := Some pool;
    Ok ()
;;

let with_pool f =
  match !global_pool with
  | None ->
    (* Fail safely into the Deferred.Result monad *)
    Deferred.Result.fail
      (Caqti_error.connect_failed
         ~uri:Uri.empty
         (Caqti_error.Msg "failed to connect to database"))
  | Some pool -> Caqti_async.Pool.use f pool
;;

let create_market_stub_table () : unit Deferred.Or_error.t =
  let%bind success_or_error =
    with_pool (fun (module C : Caqti_async.CONNECTION) ->
      C.exec Database_commands.create_market_stub_table ())
  in
  match success_or_error with
  | Ok () -> return (Ok ())
  | Error e -> Deferred.Or_error.error_string (Caqti_error.show e)
;;

let insert_market_stub market_stub : unit Deferred.Or_error.t =
  let%bind success_or_error =
    with_pool (fun (module C : Caqti_async.CONNECTION) ->
      C.exec Database_commands.insert_market_stub market_stub)
  in
  match success_or_error with
  | Ok () -> return (Ok ())
  | Error e -> Deferred.Or_error.error_string (Caqti_error.show e)
;;

let find_market_stub market_id : Market_stub.t option Deferred.Or_error.t =
  let%bind stub_or_error =
    with_pool (fun (module C : Caqti_async.CONNECTION) ->
      C.find_opt Database_commands.find_market_stub market_id)
  in
  match stub_or_error with
  | Ok stub -> return (Ok stub)
  | Error e -> Deferred.Or_error.error_string (Caqti_error.show e)
;;

let insert_market_stubs stubs : unit Deferred.Or_error.t =
  Deferred.Or_error.List.iter stubs ~how:`Sequential ~f:insert_market_stub
;;

let find_market_stub_by_slug slug : Market_stub.t option Deferred.Or_error.t =
  let%bind stub_or_error =
    with_pool (fun (module C : Caqti_async.CONNECTION) ->
      C.find_opt
        Database_commands.find_market_stub_by_slug
        (Slug.to_string slug))
  in
  match stub_or_error with
  | Ok stub -> return (Ok stub)
  | Error e -> Deferred.Or_error.error_string (Caqti_error.show e)
;;

let clear_market_stubs () : unit Deferred.Or_error.t =
  let%bind success_or_error =
    with_pool (fun (module C : Caqti_async.CONNECTION) ->
      C.exec Database_commands.delete_market_stubs ())
  in
  match success_or_error with
  | Ok () -> return (Ok ())
  | Error e -> Deferred.Or_error.error_string (Caqti_error.show e)
;;

let list_market_stubs_split split ~time ~limit =
  let split_time = Database_types.time_ns_to_int64 time in
  let command =
    match split with
    | `Current -> Database_commands.list_market_stubs_after
    | `Historical -> Database_commands.list_market_stubs_before
  in
  let%bind stubs_or_error =
    with_pool (fun (module C : Caqti_async.CONNECTION) ->
      C.collect_list command (split_time, limit))
  in
  match stubs_or_error with
  | Ok stubs -> return (Ok stubs)
  | Error e -> Deferred.Or_error.error_string (Caqti_error.show e)
;;

let list_current_market_stubs limit : Market_stub.t list Deferred.Or_error.t =
  list_market_stubs_split `Current ~time:(Time_ns.now ()) ~limit
;;

let list_historical_market_stubs limit
  : Market_stub.t list Deferred.Or_error.t
  =
  list_market_stubs_split `Historical ~time:(Time_ns.now ()) ~limit
;;

module For_testing = struct
  let list_current_market_stubs limit ~time =
    list_market_stubs_split `Current ~time ~limit
  ;;

  let list_historical_market_stubs limit ~time =
    list_market_stubs_split `Historical ~time ~limit
  ;;
end
