open! Core
open Async
open Types

let () =
  Command.async
    ~summary:"Retrieve markets from Kalshi"
    (let%map_open.Command limit = anon (maybe ("limit" %: int)) in
     fun () ->
       let%map result = Api_hitter.fetch_kalshi_markets ?limit () in
       match result with
       | Ok payload -> print_s (Raw_payload.sexp_of_t payload)
       | Error e -> Core.eprint_s [%sexp (e : Error.t)])
  |> Command_unix.run
;;
