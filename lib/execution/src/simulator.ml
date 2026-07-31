open! Core
open! Async
open Types

module Fill_model = struct
  type t =
    | At_limit
    | Against_live_book
  [@@deriving sexp_of]
end

module Position = struct
  type t =
    { mutable yes : Size.t
    ; mutable no : Size.t
    }
  [@@deriving sexp_of]

  let empty () = { yes = Size.zero; no = Size.zero }

  let get t ~contract =
    match (contract : Contract_type.t) with Yes -> t.yes | No -> t.no
  ;;

  let add t ~contract ~size =
    match (contract : Contract_type.t) with
    | Yes -> t.yes <- Size.( + ) t.yes size
    | No -> t.no <- Size.( + ) t.no size
  ;;
end

type t =
  { fill_model : Fill_model.t
  ; mutable cash : Price.t
  ; positions : Position.t Market_id.Table.t
  }
[@@deriving sexp_of]

let create ~fill_model ~starting_cash =
  { fill_model; cash = starting_cash; positions = Market_id.Table.create () }
;;

let cash t = t.cash

let position t ~market_id ~contract =
  match Hashtbl.find t.positions market_id with
  | None -> Size.zero
  | Some position -> Position.get position ~contract
;;

(* Book-keeping shared by both fill models: move the cash and the position
   the fill implies. The fill was validated by the caller; the only check
   left is affordability. *)
let settle t (fill : Fill.t) =
  let cost = Fill.cost fill in
  if Price.( > ) cost t.cash
  then
    Or_error.error_s
      [%message
        "insufficient simulated cash"
          (cost : Price.t)
          ~cash:(t.cash : Price.t)
          (fill.order : Order.t)]
  else (
    t.cash <- Price.( - ) t.cash cost;
    let position =
      Hashtbl.find_or_add
        t.positions
        fill.order.market.market_id
        ~default:Position.empty
    in
    let signed_size =
      match fill.order.side with
      | Buy -> fill.filled_size
      | Sell -> Size.neg fill.filled_size
    in
    Position.add position ~contract:fill.order.contract ~size:signed_size;
    Ok fill)
;;

let fill_at_limit t (order : Order.t) =
  match order.side with
  | Buy ->
    settle
      t
      { Fill.order
      ; filled_size = order.size
      ; price = order.limit_price
      ; fee = Price.zero
      ; venue_order_id = None
      }
  | Sell ->
    let held =
      position t ~market_id:order.market.market_id ~contract:order.contract
    in
    if Size.( < ) held order.size
    then
      Or_error.error_s
        [%message
          "cannot sell more than held; the simulator does not short"
            (held : Size.t)
            (order : Order.t)]
    else
      settle
        t
        { Fill.order
        ; filled_size = order.size
        ; price = order.limit_price
        ; fee = Price.zero
        ; venue_order_id = None
        }
;;

let fill_against_book t (order : Order.t) ~(book : Binary_book.t) =
  match order.side with
  | Sell ->
    Or_error.error_s
      [%message
        "book-aware simulation cannot price sells: Binary_book carries ask \
         depth only"
          (order : Order.t)]
  | Buy ->
    let ask, depth =
      match order.contract with
      | Yes -> book.yes_ask, book.yes_ask_size
      | No -> book.no_ask, book.no_ask_size
    in
    if Price.( > ) ask order.limit_price
    then
      Or_error.error_s
        [%message
          "limit is below the ask; the simulator only models taker fills"
            (ask : Price.t)
            (order : Order.t)]
    else (
      let filled_size = Size.min order.size depth in
      if Size.( <= ) filled_size Size.zero
      then
        Or_error.error_s [%message "no depth at the ask" (order : Order.t)]
      else (
        let fee =
          Size.multiply_by_price
            filled_size
            (Fees.taker_fee (Order.venue order) ask)
        in
        settle
          t
          { Fill.order
          ; filled_size
          ; price = ask
          ; fee
          ; venue_order_id = None
          }))
;;

let place_order t (order : Order.t) =
  match t.fill_model with
  | At_limit -> return (fill_at_limit t order)
  | Against_live_book ->
    (match%map Market_data.Market_data_gateway.fetch_book order.market with
     | Error _ as error -> error
     | Ok book -> fill_against_book t order ~book)
;;

module For_testing = struct
  let fill_against_book = fill_against_book
end
