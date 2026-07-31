open! Core
open! Async
open! Caqti_async
open! Caqti_type
open Types

let global_pool = ref None

(* "sqlite3://" ^ an absolute path yields sqlite3:///path/to/file. The old
   version baked in a directory that doesn't exist on this checkout. *)
let uri_scheme = "sqlite3://"

let init_database db_path =
  let db_uri = Uri.of_string (uri_scheme ^ db_path) in
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

let list_current_market_stubs limit : Market_stub.t list Deferred.Or_error.t =
  let curr_time = Time_ns.now () |> Database_types.time_ns_to_int64 in
  let%bind stubs_or_error =
    with_pool (fun (module C : Caqti_async.CONNECTION) ->
      C.collect_list
        Database_commands.list_market_stubs_after
        (curr_time, limit))
  in
  match stubs_or_error with
  | Ok stubs -> return (Ok stubs)
  | Error e -> Deferred.Or_error.error_string (Caqti_error.show e)
;;

module For_testing = struct
  let list_current_market_stubs limit ~time =
    let curr_time = time |> Database_types.time_ns_to_int64 in
    let%bind stubs_or_error =
      with_pool (fun (module C : Caqti_async.CONNECTION) ->
        C.collect_list
          Database_commands.list_market_stubs_after
          (curr_time, limit))
    in
    match stubs_or_error with
    | Ok stubs -> return (Ok stubs)
    | Error e -> Deferred.Or_error.error_string (Caqti_error.show e)
  ;;
end

let create_pair_proposal_table () : unit Deferred.Or_error.t =
  let%bind success_or_error =
    with_pool (fun (module C : Caqti_async.CONNECTION) ->
      C.exec Database_commands.create_pair_proposal_table ())
  in
  match success_or_error with
  | Ok () -> return (Ok ())
  | Error e -> Deferred.Or_error.error_string (Caqti_error.show e)
;;

let propose_pair proposal : unit Deferred.Or_error.t =
  let%bind success_or_error =
    with_pool (fun (module C : Caqti_async.CONNECTION) ->
      C.exec Database_commands.insert_pair_proposal proposal)
  in
  match success_or_error with
  | Ok () -> return (Ok ())
  | Error e -> Deferred.Or_error.error_string (Caqti_error.show e)
;;

let set_pair_status ~left ~right status : unit Deferred.Or_error.t =
  let%bind success_or_error =
    with_pool (fun (module C : Caqti_async.CONNECTION) ->
      C.exec Database_commands.set_pair_status (status, left, right))
  in
  match success_or_error with
  | Ok () -> return (Ok ())
  | Error e -> Deferred.Or_error.error_string (Caqti_error.show e)
;;

let list_pair_proposals_by_status status
  : Pair_proposal.t list Deferred.Or_error.t
  =
  let%bind pairs_or_error =
    with_pool (fun (module C : Caqti_async.CONNECTION) ->
      C.collect_list Database_commands.list_pair_proposals_by_status status)
  in
  match pairs_or_error with
  | Ok pairs -> return (Ok pairs)
  | Error e -> Deferred.Or_error.error_string (Caqti_error.show e)
;;
