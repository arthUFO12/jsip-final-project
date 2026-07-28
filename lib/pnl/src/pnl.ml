open! Core
open Types

module Expiration = struct
  type t =
    { date : Time_ns.t
    ; winner : Contract_type.t
    }
end

type position =
  { inventory : Size.t
  ; realized_pnl : Price.t
  ; cost_basis : Price.t
  }

type slug_data =
  { mutable yes_bid_price : Price.t
  ; mutable no_bid_price : Price.t
  ; mutable yes_ask_price : Price.t
  ; mutable no_ask_price : Price.t
  ; expiration : Expiration.t option
  }

let flat_position =
  { inventory = Size.zero
  ; realized_pnl = Price.zero
  ; cost_basis = Price.zero
  }
;;

type t =
  { data : slug_data Slug.Table.t
  ; positions : position Slug.Table.t
  ; mutable cash : Price.t
  }

let convert_transaction_to_delta
  size
  (side : Side.t)
  (contract_type : Contract_type.t)
  =
  match side, contract_type with
  | Buy, Yes | Sell, No -> size
  | Sell, Yes | Buy, No -> Size.neg size
;;

let realized_on_reduce ~closed ~trade_price ~cost_basis_removed =
  Price.( - ) (Size.multiply_by_price closed trade_price) cost_basis_removed
;;

let realize_on_expiry size = Size.multiply_by_price size Price.one

let realized_expired_bets t curr_time= 
  Hashtbl.fold t.data ~f:(fun ~key:slug ~data:slug_data acc -> 
    let position = Hashtbl.find_or_add t.positions slug ~default:flat_position in
    match slug_data.expiration with 
    | None -> acc 
    | Some expiry -> 
      if Time_ns.(curr_time > expiry.date) then 
        let realized_winnings = realize_on_expiry position.inventory in 
        Hashtbl.set t.positions ~key:slug ~data:{ position with inventory = Size.zero;  }
        
    )
let calculate_unrealized_bet_revenue position data =
  match Size.sign position.inventory with
  | Pos -> Size.multiply_by_price position.inventory data.yes_bid_price
  | _ -> Size.multiply_by_price position.inventory data.no_bid_price
;;

let unrealized_pnl t =
  Hashtbl.fold t.data ~init:Price.zero ~f:(fun ~key:slug ~data acc ->
    let position =
      Hashtbl.find_or_add t.positions slug ~default:(fun () -> flat_position)
    in
    let bet_pnl =
      Price.(
        calculate_unrealized_bet_revenue position data - position.cost_basis)
    in
    Price.(acc + bet_pnl))
;;

let realized_pnl t =
  Hashtbl.fold
    t.positions
    ~init:Price.zero
    ~f:(fun ~key:_ ~data:position acc -> Price.(acc + position.realized_pnl))
;;

let cash t = t.cash

let apply_trade position ~quantity_change ~yes_price ~no_price =
  let { inventory; cost_basis; realized_pnl; payout } = position in
  match
    Size.equal inventory Size.zero
    || Sign.equal (Size.sign inventory) (Size.sign quantity_change)
  with
  | true ->
    let price =
      match Size.sign quantity_change with Pos -> yes_price | _ -> no_price
    in
    let cost_basis_added =
      Size.multiply_by_price (Size.abs quantity_change) price
    in
    ( { position with
        inventory = Size.(inventory + quantity_change)
      ; cost_basis = Price.(cost_basis + cost_basis_added)
      }
    , Price.neg cost_basis_added )
  | false ->
    let closed = Size.min (Size.abs quantity_change) (Size.abs inventory) in
    let close_price, open_price =
      match Size.sign quantity_change with
      | Pos -> no_price, yes_price
      | _ -> yes_price, no_price
    in
    let cost_basis_removed =
      Size.divide_price
        (Size.multiply_by_price closed cost_basis)
        (Size.abs inventory)
    in
    let realized_delta =
      realized_on_reduce ~closed ~trade_price:close_price ~cost_basis_removed
    in
    let realized_pnl = Price.(realized_pnl + realized_delta) in
    (match Size.( <= ) (Size.abs quantity_change) (Size.abs inventory) with
     | true ->
       ( { inventory = Size.(inventory + quantity_change)
         ; cost_basis = Price.(cost_basis - cost_basis_removed)
         ; realized_pnl
         ; payout = Price.zero
         }
       , cost_basis_removed )
     | false ->
       let remainder = Size.(inventory + quantity_change) in
       let cost_basis_added = Size.multiply_by_price remainder open_price in
       ( { inventory = remainder
         ; cost_basis = cost_basis_added
         ; realized_pnl
         ; payout = Price.zero
         }
       , Price.(cost_basis_removed - cost_basis_added) ))
;;

let update_position
  t
  ~slug
  ~(side : Side.t)
  ~(contract_type : Contract_type.t)
  ~size
  =
  let quantity_change =
    convert_transaction_to_delta size side contract_type
  in
  let data = Hashtbl.find_exn t.data slug in
  let yes_price, no_price =
    match Size.sign quantity_change with
    | Pos -> data.yes_ask_price, data.no_bid_price
    | _ -> data.yes_bid_price, data.no_ask_price
  in
  let slug_position, cash_delta =
    Hashtbl.find t.positions slug
    |> Option.value ~default:flat_position
    |> apply_trade ~quantity_change ~yes_price ~no_price
  in
  t.cash <- Price.( + ) t.cash cash_delta;
  Hashtbl.set t.positions ~key:slug ~data:slug_position
;;

let apply_trade_report
  ~slug
  ?(yes_bbo : Bbo.t option)
  ?(no_bbo : Bbo.t option)
  t
  =
  let old = Hashtbl.find_exn t.data slug in
  (match yes_bbo with
   | None -> ()
   | Some bbo ->
     old.yes_bid_price <- bbo.bid;
     old.yes_ask_price <- bbo.ask);
  match no_bbo with
  | None -> ()
  | Some bbo ->
    old.no_bid_price <- bbo.bid;
    old.no_ask_price <- bbo.ask
;;
