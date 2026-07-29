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
  ; mutable expiration : Expiration.t option
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

let create slugs cash =
  let data = Slug.Table.create () in
  List.iter slugs ~f:(fun slug ->
    Hashtbl.set
      data
      ~key:slug
      ~data:
        { yes_bid_price = Price.zero
        ; no_bid_price = Price.zero
        ; yes_ask_price = Price.zero
        ; no_ask_price = Price.zero
        ; expiration = None
        });
  { data; positions = Slug.Table.create (); cash }
;;

let mem t slug = Hashtbl.mem t.data slug

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
  Price.(Size.multiply_by_price closed trade_price - cost_basis_removed)
;;

let realize_winnings (position : position) (expiry : Expiration.t) curr_time =
  if Time_ns.(curr_time > expiry.date)
  then (
    let position_side =
      if Size.(position.inventory >= Size.zero)
      then Contract_type.Yes
      else Contract_type.No
    in
    match expiry.winner, position_side with
    | Yes, Yes | No, No ->
      let winnings =
        Size.multiply_by_price (Size.abs position.inventory) Price.one
      in
      let winnings_pnl = Price.(winnings - position.cost_basis) in
      ( winnings
      , { inventory = Size.zero
        ; cost_basis = Price.zero
        ; realized_pnl = Price.(position.realized_pnl + winnings_pnl)
        } )
    | _ ->
      ( Price.zero
      , { inventory = Size.zero
        ; cost_basis = Price.zero
        ; realized_pnl = Price.(position.realized_pnl - position.cost_basis)
        } ))
  else Price.zero, position
;;

let set_expiration t ~slug ~date ~winner =
  let data = Hashtbl.find_exn t.data slug in
  data.expiration <- Some { date; winner }
;;

let settle_expired_bets t ~now =
  Hashtbl.iteri t.data ~f:(fun ~key:slug ~data:slug_data ->
    match slug_data.expiration with
    | None -> ()
    | Some expiry ->
      let position =
        Hashtbl.find_or_add t.positions slug ~default:(fun () ->
          flat_position)
      in
      let winnings, new_position = realize_winnings position expiry now in
      t.cash <- Price.(t.cash + winnings);
      Hashtbl.set t.positions ~key:slug ~data:new_position)
;;

(* [inventory] encodes direction in its sign; revenue is always the magnitude
   marked at the side we could sell into. *)
let calculate_unrealized_bet_revenue position data =
  match Size.sign position.inventory with
  | Pos -> Size.multiply_by_price position.inventory data.yes_bid_price
  | _ ->
    Size.multiply_by_price (Size.abs position.inventory) data.no_bid_price
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
  let { inventory; cost_basis; realized_pnl } = position in
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
    let proceeds = Size.multiply_by_price closed close_price in
    (match Size.( <= ) (Size.abs quantity_change) (Size.abs inventory) with
     | true ->
       ( { inventory = Size.(inventory + quantity_change)
         ; cost_basis = Price.(cost_basis - cost_basis_removed)
         ; realized_pnl
         }
       , proceeds )
     | false ->
       let remainder = Size.(inventory + quantity_change) in
       let cost_basis_added =
         Size.multiply_by_price (Size.abs remainder) open_price
       in
       ( { inventory = remainder
         ; cost_basis = cost_basis_added
         ; realized_pnl
         }
       , Price.(proceeds - cost_basis_added) ))
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
