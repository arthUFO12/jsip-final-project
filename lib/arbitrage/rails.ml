open! Core
open! Async
open! Types

let order_dollars (order : Execution.Order.t) =
  let notional = Size.multiply_by_price order.size order.limit_price in
  Float.of_int (Price.to_microcents notional) /. 100_000_000.
;;

let utc_day_start () =
  let zone = Time_float.Zone.utc in
  Time_ns.of_date_ofday
    ~zone
    (Time_ns.to_date (Time_ns.now ()) ~zone)
    Time_ns.Ofday.start_of_day
;;

let log_live_order
  (order : Execution.Order.t)
  ~client_order_id
  ~outcome
  ~dollars
  =
  let entry =
    { Trade_log_entry.at = Time_ns.now ()
    ; venue = Venue.to_string (Execution.Order.venue order)
    ; market_id = order.market.market_id
    ; action = "place"
    ; client_order_id = Some client_order_id
    ; outcome
    ; detail =
        Sexp.to_string
          [%message
            ""
              ~side:(order.side : Side.t)
              ~size:(order.size : Size.t)
              ~contract:(order.contract : Contract_type.t)
              ~limit:(Price.to_string_dollar order.limit_price : string)
              ~title:(order.market.title : string)]
    ; dollars
    }
  in
  match%map Database.Database_exec.append_trade_log entry with
  | Ok () -> ()
  | Error error ->
    (* The audit write failing must not strand a live position half-made, but
       it must be impossible to miss. *)
    Core.eprint_s
      [%message
        "TRADE LOG WRITE FAILED - audit trail is incomplete"
          (entry : Trade_log_entry.t)
          (error : Error.t)]
;;

(* The rails between deciding and spending: the kill switch and both spending
   caps are checked immediately before every live order. [Ok] means clear to
   send. *)
let live_refusal
  ~(execution : Config.Execution.t)
  (order : Execution.Order.t)
  =
  match%bind Execution.Kill_switch.engaged () with
  | Some reason -> return (Some [%string "kill switch engaged: %{reason}"])
  | None ->
    let dollars = order_dollars order in
    let cap_dollars price =
      Float.of_int (Price.to_microcents price) /. 100_000_000.
    in
    let per_order = cap_dollars execution.max_dollars_per_order in
    if Float.( > ) dollars per_order
    then
      return
        (Some
           [%string
             "order notional $%{dollars#Float} exceeds \
              max_dollars_per_order $%{per_order#Float}"])
    else (
      match%map
        Database.Database_exec.sum_trade_dollars_since (utc_day_start ())
      with
      | Error error ->
        (* Fail closed: an unreadable ledger means the cap cannot be
           enforced, so the order must not go out. *)
        Some
          [%string
            "cannot read trade log to enforce daily cap: \
             %{Error.to_string_hum error}"]
      | Ok spent_today ->
        let per_day = cap_dollars execution.max_dollars_per_day in
        (match Float.( > ) (spent_today +. dollars) per_day with
         | true ->
           Some
             [%string
               "daily cap: $%{spent_today#Float} already placed today, this \
                order's $%{dollars#Float} would exceed max_dollars_per_day \
                $%{per_day#Float}"]
         | false -> None))
;;
