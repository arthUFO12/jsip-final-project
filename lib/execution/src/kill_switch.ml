open! Core
open! Async

let env_var = "TRADING_DISABLED"
let sentinel_file = "trading.disabled"

let engaged () =
  match Core.Sys.getenv env_var with
  | Some (_ : string) ->
    return (Some [%string "%{env_var} is set in the environment"])
  | None ->
    (match%map Sys.file_exists_exn sentinel_file with
     | true ->
       Some [%string "%{sentinel_file} exists in the working directory"]
     | false -> None)
;;
