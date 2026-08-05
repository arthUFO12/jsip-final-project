(* Replays the arbitrage strategy over the approved pairs' two-venue price
   history, one RPC per backtest — the arb pipeline's analogue of
   {!Sim_runner}. Each pair's legs are fetched as mid-price series
   ({!Market_data_gateway}, venue-dispatched: Kalshi candlesticks,
   Polymarket prices-history), aligned on a shared interpolated grid, and
   re-priced every tick with the scan's own fee math via
   {!Arbitrage.Replay}. Pairs that cannot be replayed (a leg without a
   history handle, no overlapping data) are skipped with a reason, never
   failing the run.

   Model bias, restated wherever results surface: history has no order
   books, so edges are mid-price upper bounds on what the live scan —
   which pays real asks — would have found, and [stake] is an assumption,
   not recorded depth. *)

open! Core
open! Async
open Types
open Market_data

let max_lookback_days = 30
let max_stake = 1_000
let label_width = 44

let validate
  ({ lookback_days; interval = _; stake; min_edge_cents } :
    Protocol.Arb_sim_request.t)
  =
  if lookback_days < 1 || lookback_days > max_lookback_days
  then
    Or_error.error_s
      [%message
        "lookback must be between 1 and 30 days" (lookback_days : int)]
  else if stake < 1 || stake > max_stake
  then
    Or_error.error_s
      [%message "stake must be between 1 and 1000 contracts" (stake : int)]
  else if min_edge_cents < 0 || min_edge_cents > 99
  then
    Or_error.error_s
      [%message
        "min edge must be between 0 and 99 cents" (min_edge_cents : int)]
  else Ok ()
;;

let interval_of_wire : Protocol.Interval.t -> Time_series.Interval.t
  = function
  | Minute -> Minute
  | Hour -> Hour
  | Day -> Day
;;

let history_capable (stub : Market_stub.t) =
  match stub.venue with
  | Kalshi -> Option.is_some stub.series_ticker
  | Polymarket -> Option.is_some stub.clob_token_id
;;

(* Same key the scan and sweep use: both market ids, sorted. *)
let pair_key_of (left : Market_stub.t) (right : Market_stub.t) =
  String.concat
    ~sep:"|"
    (List.sort
       ~compare:String.compare
       [ Market_id.to_string left.market_id
       ; Market_id.to_string right.market_id
       ])
;;

let truncate_label text =
  match String.length text <= label_width with
  | true -> text
  | false -> String.prefix text (label_width - 1) ^ "…"
;;

let epoch_s time = Time_ns.Span.to_sec (Time_ns.to_span_since_epoch time)

let episode_of_replay (episode : Arbitrage.Replay.Episode.t) =
  { Protocol.Arb_episode.entered_s = episode.entered_s
  ; exited_s = episode.exited_s
  ; entry_edge = episode.entry_edge
  ; locked_dollars = episode.locked_dollars
  }
;;

(* One pair's replay. [Error] here means "skip this pair, with a reason". *)
let simulate_pair
  ~start
  ~finish
  ~interval
  ~min_edge
  ~stake
  ({ left; right } : Arbitrage.Matcher.Candidate.t)
  =
  let open Deferred.Or_error.Let_syntax in
  match List.for_all [ left; right ] ~f:history_capable with
  | false ->
    Deferred.return
      (Or_error.error_string
         "a leg cannot serve price history (missing venue handle)")
  | true ->
    let%bind fetched =
      Market_data_gateway.fetch_many_ticker_series
        [ left; right ]
        ~start
        ~finish
        ~interval
    in
    let series_of (stub : Market_stub.t) =
      match
        List.find fetched ~f:(fun ((fetched_stub : Market_stub.t), _) ->
          Market_id.equal fetched_stub.market_id stub.market_id)
      with
      | Some ((_ : Market_stub.t), series) -> Ok series
      | None ->
        Or_error.error_s
          [%message "leg missing from fetch" (stub.slug : Slug.t)]
    in
    let%bind window_start, window_finish =
      Deferred.return (Time_series.shared_window fetched)
    in
    let aligned stub =
      let open Or_error.Let_syntax in
      let%bind series = series_of stub in
      Time_series.interpolate
        series
        ~start:window_start
        ~finish:window_finish
        ~interval
    in
    let%bind left_series = Deferred.return (aligned left) in
    let%bind right_series = Deferred.return (aligned right) in
    let%bind zipped =
      match List.zip left_series right_series with
      | Ok zipped -> return zipped
      | Unequal_lengths ->
        Deferred.Or_error.error_string
          "aligned series disagree on grid length"
    in
    let points =
      List.map
        zipped
        ~f:(fun
             ( (point_a : Time_series.Point.t)
             , (point_b : Time_series.Point.t) )
           ->
          ( epoch_s point_a.time
          , Arbitrage.Replay.edge_after_fees
              ~venue_a:left.venue
              ~yes_a:point_a.yes_price
              ~venue_b:right.venue
              ~yes_b:point_b.yes_price ))
    in
    let episodes = Arbitrage.Replay.episodes ~points ~min_edge ~stake in
    let cumulative = Arbitrage.Replay.cumulative episodes in
    let total_dollars =
      List.sum
        (module Float)
        episodes
        ~f:(fun episode -> episode.locked_dollars)
    in
    return
      { Protocol.Arb_pair_series.pair_key = pair_key_of left right
      ; label = truncate_label left.title
      ; points
      ; episodes = List.map episodes ~f:episode_of_replay
      ; cumulative
      ; total_dollars
      }
;;

let run (request : Protocol.Arb_sim_request.t) =
  let open Deferred.Or_error.Let_syntax in
  let%bind () = Deferred.return (validate request) in
  let%bind candidates = Arbitrage.Bot.candidates_of_approved () in
  match candidates with
  | [] ->
    Deferred.Or_error.error_string
      "no approved pairs to replay - run a sweep and approve some first"
  | candidates ->
    let finish = Time_ns.now () in
    let start =
      Time_ns.sub
        finish
        (Time_ns.Span.of_day (Float.of_int request.lookback_days))
    in
    let interval = interval_of_wire request.interval in
    let min_edge = Float.of_int request.min_edge_cents /. 100. in
    (* Sequential on purpose: each pair is two venue fetches and the
       gateway already paces them a second apart. *)
    let%bind pairs, skipped =
      Deferred.ok
        (Deferred.List.fold
           candidates
           ~init:([], [])
           ~f:(fun (pairs, skipped) candidate ->
             match%map.Deferred
               simulate_pair
                 ~start
                 ~finish
                 ~interval
                 ~min_edge
                 ~stake:request.stake
                 candidate
             with
             | Ok series -> series :: pairs, skipped
             | Error error ->
               ( pairs
               , (truncate_label candidate.left.title
                 , Error.to_string_hum error)
                 :: skipped )))
    in
    let pairs = List.rev pairs in
    let combined =
      List.concat_map pairs ~f:(fun pair -> pair.episodes)
      |> List.sort
           ~compare:(fun (a : Protocol.Arb_episode.t) b ->
             Float.compare a.entered_s b.entered_s)
      |> List.folding_map ~init:0. ~f:(fun total episode ->
        let total = total +. episode.Protocol.Arb_episode.locked_dollars in
        total, (episode.entered_s, total))
    in
    return
      { Protocol.Arb_sim_result.pairs
      ; combined
      ; total_dollars =
          List.sum
            (module Float)
            pairs
            ~f:(fun pair -> pair.total_dollars)
      ; episode_count =
          List.sum
            (module Int)
            pairs
            ~f:(fun pair -> List.length pair.episodes)
      ; skipped = List.rev skipped
      }
;;
