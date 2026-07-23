open! Core
open Async
open Types

val init_database : unit -> unit Or_error.t
val create_market_stub_table : unit -> unit Deferred.Or_error.t
val insert_market_stub : Market_stub.t -> unit Deferred.Or_error.t

val find_market_stub
  :  Market_id.t
  -> Market_stub.t option Deferred.Or_error.t

val list_current_market_stubs : int -> Market_stub.t list Deferred.Or_error.t
