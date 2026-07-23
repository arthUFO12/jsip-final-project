open! Core
open Async
open Types

let () =
  Command.async
    ~summary:"Retrieve markets from Kalshi"
    (let%map_open.Command limit = anon (maybe ("limit" %: int)) in
     fun () ->
       let%map (raw_kalshi_output : Types.Raw_payload.t) =
         Api_hitter.fetch_kalshi_markets ?limit ()
       in
       print_s (Raw_payload.sexp_of_t raw_kalshi_output))
  |> Command_unix.run
;;
