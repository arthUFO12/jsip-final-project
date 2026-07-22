open! Core
open Async
open Types

let fetch_markets ?limit:_ ~venue:_ () =
  Deferred.Or_error.error_string "TODO: fetch_markets"
;;
