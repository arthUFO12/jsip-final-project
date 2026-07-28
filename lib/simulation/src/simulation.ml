open! Core

type 'a bot = (module Bot_interface.Bot with type Config.t = 'a)
type packed_bot = P : 'a bot * 'a -> packed_bot

let get_data _market_stubs (P (m, _bot)) =
  let module Bot_module = (val m) in
  ()
;;
