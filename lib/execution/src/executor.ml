open! Core
open! Async
open! Types

type t =
  | Paper of Simulator.t
  | Live of Kalshi_live.Credentials.t

let paper simulator = Paper simulator
let live credentials = Live credentials
let is_live = function Paper (_ : Simulator.t) -> false | Live _ -> true

let place_order ?client_order_id t (order : Order.t) =
  match t with
  | Paper simulator -> Simulator.place_order simulator order
  | Live credentials ->
    (match Order.venue order with
     | Kalshi -> Kalshi_live.place_order ?client_order_id credentials order
     | Polymarket ->
       Deferred.Or_error.error_s
         [%message
           "polymarket live trading is not supported: order placement needs \
            an EIP-712 wallet signature; run paper mode for polymarket legs"
             (order : Order.t)])
;;
