open! Core

let int_in ~min ~max ~what text =
  match Int.of_string_opt (String.strip text) with
  | None -> Error [%string "%{what} must be a whole number"]
  | Some value ->
    (match value >= min && value <= max with
     | true -> Ok value
     | false ->
       Error [%string "%{what} must be between %{min#Int} and %{max#Int}"])
;;

let lookback_days = int_in ~min:1 ~max:30 ~what:"lookback"
let warmup_hours = int_in ~min:0 ~max:719 ~what:"warmup"

let warmup_within_lookback ~warmup_hours ~lookback_days =
  match warmup_hours >= lookback_days * 24 with
  | false -> None
  | true -> Some "warmup must be shorter than the lookback window"
;;

let starting_cash_dollars =
  int_in ~min:1 ~max:1_000_000 ~what:"starting cash"
;;

let bot_count = int_in ~min:1 ~max:100 ~what:"number of bots"
let bot_max_size = int_in ~min:1 ~max:1000 ~what:"max trade size"

let trade_probability text =
  match Float.of_string_opt (String.strip text) with
  | None -> Error "trade probability must be a number"
  | Some value ->
    (match Float.O.(value > 0. && value <= 1.) with
     | true -> Ok value
     | false ->
       Error "trade probability must be above 0 and at most 1")
;;

let match_threshold text =
  match Float.of_string_opt (String.strip text) with
  | None -> Error "threshold must be a number"
  | Some value ->
    (match Float.O.(value >= 0. && value <= 1.) with
     | true -> Ok value
     | false -> Error "threshold must be between 0 and 1")
;;

let fill_price_cents = int_in ~min:1 ~max:99 ~what:"fill price (cents)"
let fill_count = int_in ~min:1 ~max:10_000 ~what:"contracts filled"

let definition_collision ~draft ~variables =
  match Rule_candidates.definition_name (String.strip draft) with
  | None -> None
  | Some name ->
    let taken =
      Rule_candidates.builtins
      @ List.filter_map variables ~f:Rule_candidates.definition_name
    in
    (match List.mem taken name ~equal:String.equal with
     | false -> None
     | true ->
       Some [%string "%{name} is already defined — pick another name"])
;;
