open! Core
open! Async
open Types

let fetch_l1_market_data ?events_limit ~(venue : Venue.t) ~closed ~limit () =
  let%bind.Deferred.Or_error payload =
    match (venue : Venue.t) with
    | Polymarket -> Fetch_live.fetch_polymarket_markets ~closed ()
    | Kalshi ->
      let status = if closed then "closed" else "open" in
      Fetch_live.fetch_kalshi_markets ?limit:events_limit ~status ()
  in
  Deferred.return (Live_data_parser.parse_data ~limit ~venue payload.body)
;;

let fetch_one_ticker_series
  (market_stub : Market_stub.t)
  ~start
  ~finish
  ~interval
  =
  match market_stub.venue with
  | Kalshi ->
    let%bind.Deferred.Or_error response_body =
      Fetch_time_series.fetch_kalshi_data
        market_stub
        ~start
        ~finish
        ~interval
    in
    Deferred.Or_error.return
      (Time_series_parser.parse_kalshi_time_series response_body)
  | Polymarket ->
    let%bind.Deferred.Or_error response_body =
      Fetch_time_series.fetch_polymarket_data
        market_stub
        ~start
        ~finish
        ~interval
    in
    Deferred.Or_error.return
      (Time_series_parser.parse_polymarket_time_series response_body)
;;

let fetch_many_ticker_series
  (market_stubs : Market_stub.t list)
  ~start
  ~finish
  ~interval
  : (Market_stub.t * Time_series.t) list Deferred.Or_error.t
  =
  let%bind.Deferred.Or_error time_series =
    List.map market_stubs ~f:(fun stub ->
      let%bind () = Clock.after (Time_float.Span.of_sec 1.) in
      fetch_one_ticker_series stub ~start ~finish ~interval)
    |> Deferred.Or_error.all
  in
  Deferred.Or_error.return (List.zip_exn market_stubs time_series)
;;

let search_polymarket ~query =
  let%bind.Deferred.Or_error payload =
    Fetch_live.fetch_polymarket_search ~query ()
  in
  Deferred.return (Live_data_parser.parse_polymarket_search payload.body)
;;

(* Backstops are exhaustive by definition, so cap pages loudly rather than
   truncate silently. *)
let max_pages = 25

let fetch_all_kalshi () =
  let open Deferred.Or_error.Let_syntax in
  let events_per_page = 200 in
  let rec loop ~cursor ~pages_fetched acc =
    let%bind payload =
      Fetch_live.fetch_kalshi_markets ~limit:events_per_page ?cursor ()
    in
    let%bind markets =
      Deferred.return
        (Live_data_parser.parse_data ~venue:Venue.Kalshi payload.body)
    in
    let acc = acc @ [ markets ] in
    match Live_data_parser.parse_kalshi_cursor payload.body with
    | None -> return (List.concat acc)
    | Some _ when pages_fetched >= max_pages ->
      Core.eprintf
        "fetch_all_kalshi: stopped at the %d-page cap with pages left\n"
        max_pages;
      return (List.concat acc)
    | Some cursor ->
      loop ~cursor:(Some cursor) ~pages_fetched:(pages_fetched + 1) acc
  in
  loop ~cursor:None ~pages_fetched:1 []
;;

let fetch_all_polymarket () =
  let open Deferred.Or_error.Let_syntax in
  (* Gamma serves at most 100 markets per page and refuses offsets past 2500,
     so the reachable catalog is its 2500 most-traded markets when ordered by
     volume — which is exactly how a backstop should spend a bounded listing. *)
  let markets_per_page = 100 in
  (* No cursor here; page by offset until a page comes back empty — robust
     even when some entries on a page fail to parse. *)
  (* Gamma rejects burst paging (422s that succeed on retry) and refuses
     offsets past a few thousand outright, so pace the pages, retry with a
     pause, and past retries keep what we have — loudly. *)
  let page_pause = Time_ns.Span.of_sec 1. in
  let max_attempts_per_page = 3 in
  let rec loop ~offset ~pages_fetched ~attempts acc =
    match%bind.Deferred
      let%bind payload =
        Fetch_live.fetch_polymarket_markets
          ~limit:markets_per_page
          ~offset
          ~order_by_24h_volume:true
          ()
      in
      Deferred.return
        (Live_data_parser.parse_data ~venue:Venue.Polymarket payload.body)
    with
    | Error (_ : Error.t) when offset > 0 && attempts < max_attempts_per_page
      ->
      let%bind () = Deferred.ok (Clock_ns.after page_pause) in
      loop ~offset ~pages_fetched ~attempts:(attempts + 1) acc
    | Error error when offset > 0 ->
      Core.eprint_s
        [%message
          "fetch_all_polymarket: deeper page refused; continuing with \
           markets fetched so far"
            (offset : int)
            (error : Error.t)];
      return (List.concat acc)
    | Error _ as error -> Deferred.return error
    | Ok [] -> return (List.concat acc)
    | Ok markets when pages_fetched >= max_pages ->
      Core.eprintf
        "fetch_all_polymarket: stopped at the %d-page cap with pages left\n"
        max_pages;
      return (List.concat (acc @ [ markets ]))
    | Ok markets ->
      let%bind () = Deferred.ok (Clock_ns.after page_pause) in
      loop
        ~offset:(offset + markets_per_page)
        ~pages_fetched:(pages_fetched + 1)
        ~attempts:1
        (acc @ [ markets ])
  in
  loop ~offset:0 ~pages_fetched:1 ~attempts:1 []
;;

let fetch_all_l1_market_data ~(venue : Venue.t) =
  match venue with
  | Kalshi -> fetch_all_kalshi ()
  | Polymarket -> fetch_all_polymarket ()
;;
