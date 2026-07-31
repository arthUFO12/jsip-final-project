open! Core
open! Types

module Leg = struct
  type t =
    { venue : Venue.t
    ; market_id : Market_id.t
    ; book : Binary_book.t
    }
  [@@deriving sexp_of]
end

module Entry = struct
  type t =
    { venue : Venue.t
    ; market_id : Market_id.t
    ; price : Price.t
    }
  [@@deriving sexp_of]

  let of_leg ({ venue; market_id; book } : Leg.t) ~contract =
    let price =
      match (contract : Contract_type.t) with
      | Yes -> book.yes_ask
      | No -> book.no_ask
    in
    { venue; market_id; price }
  ;;
end

type opportunity =
  { yes : Entry.t
  ; no : Entry.t
  ; cost : Price.t
  ; edge : Price.t
  ; size : Size.t
  }
[@@deriving sexp_of]

module Mode = struct
  type t =
    | Exact
    | Reckless
  [@@deriving sexp_of]

  let include_fees = function Exact -> true | Reckless -> false
  let require_size = function Exact -> true | Reckless -> false
end

(* Fee logic lives in {!Execution.Fees} so the detector that prices an edge
   and the executor that charges a fill can never disagree. *)
let taker_fee = Execution.Fees.taker_fee

let cost_and_size ~mode ~(yes : Leg.t) ~(no : Leg.t) =
  let yes_ask = yes.book.yes_ask in
  let no_ask = no.book.no_ask in
  let fee venue ask =
    if Mode.include_fees mode then taker_fee venue ask else Price.zero
  in
  let cost =
    Price.(yes_ask + fee yes.venue yes_ask + no_ask + fee no.venue no_ask)
  in
  cost, Size.min yes.book.yes_ask_size no.book.no_ask_size
;;

let find ~mode left right =
  let opportunity ~yes ~no =
    let cost, size = cost_and_size ~mode ~yes ~no in
    if Price.( < ) cost Price.one
       && ((not (Mode.require_size mode)) || Size.( > ) size Size.zero)
    then
      Some
        { yes = Entry.of_leg yes ~contract:Contract_type.Yes
        ; no = Entry.of_leg no ~contract:Contract_type.No
        ; cost
        ; edge = Price.( - ) Price.one cost
        ; size
        }
    else None
  in
  [ opportunity ~yes:left ~no:right; opportunity ~yes:right ~no:left ]
  |> List.filter_opt
  |> List.max_elt
       ~compare:(Comparable.lift Price.compare ~f:(fun { edge; _ } -> edge))
;;

module For_testing = struct
  let kalshi_fee = Execution.Fees.kalshi_taker_fee
  let taker_fee = taker_fee
  let cost_and_size = cost_and_size
end
