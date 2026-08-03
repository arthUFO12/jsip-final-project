open! Core
open! Async
open! Types
open Arbitrage

(* One real fetch -> match -> detect cycle against the live venue APIs, with
   each stage's numbers printed. Text-only matching so no API key is needed;
   both detect modes so the depth gap is visible. Run with: dune exec
   lib/arbitrage/test/arb_smoke.exe *)

let scan_limit = 200

let main () =
  let open Deferred.Or_error.Let_syntax in
  let%bind per_venue =
    Deferred.Or_error.List.map
      ~how:`Sequential
      Venue.[ Kalshi; Polymarket ]
      ~f:(fun venue ->
        let%map markets =
          Market_data.Market_data_gateway.fetch_l1_market_data
            ~venue
            ~closed:false
            ~limit:scan_limit
            ()
        in
        printf
          !"fetched %{Venue}: %d markets, %d with two-sided quotes\n"
          venue
          (List.length markets)
          (List.count markets ~f:(fun l1 ->
             Option.is_some (Bot.leg_of_l1 l1)));
        (* The category mix bounds how much category blocking can prune:
           Miscellaneous is a wildcard, so only labeled markets gate. *)
        List.map markets ~f:(fun l1 -> l1.L1_market_metadata.category)
        |> List.sort_and_group ~compare:Category.compare
        |> List.map ~f:(fun group ->
          [%string "%{List.hd_exn group#Category}:%{List.length group#Int}"])
        |> String.concat ~sep:" "
        |> print_endline;
        venue, markets)
  in
  let stubs venue =
    List.Assoc.find_exn per_venue venue ~equal:Venue.equal
    |> List.map ~f:L1_market_metadata.to_market_stub
  in
  let%bind candidates =
    Deferred.ok
      (Bot.confirm_pairs
         ~matching:Config.Matching.default_text_only
         (stubs Kalshi)
         (stubs Polymarket))
  in
  printf "confirmed candidates: %d\n" (List.length candidates);
  List.iter candidates ~f:(fun { Matcher.Candidate.left; right } ->
    print_endline [%string "  %{left.title}  <->  %{right.title}"]);
  (* How close did the venues come? Show the best-scoring blocked pairs
     regardless of threshold, so a zero above is diagnosable. *)
  let blocked =
    Matcher.For_testing.block (stubs Kalshi) (stubs Polymarket)
  in
  printf
    "pairs sharing a tag: %d; highest similarity scores:\n"
    (List.length blocked);
  List.map blocked ~f:(fun candidate ->
    Matcher.For_testing.score candidate, candidate)
  |> List.sort ~compare:(fun (left, _) (right, _) ->
    Float.compare right left)
  |> (fun scored -> List.take scored 5)
  |> List.iter ~f:(fun (score, { Matcher.Candidate.left; right }) ->
    printf "  %.2f  %s  <->  %s\n" score left.title right.title);
  (* Prove the depth feed works even when no candidates exist: pull one real
     order book per venue. *)
  let%bind () =
    Deferred.ok
      (Deferred.List.iter
         ~how:`Sequential
         Venue.[ Kalshi; Polymarket ]
         ~f:(fun venue ->
           let samples =
             List.Assoc.find_exn per_venue venue ~equal:Venue.equal
             |> List.filter ~f:(fun (l1 : L1_market_metadata.t) ->
               let traded_today =
                 match l1.volume_24h with
                 | Some (Contracts contracts) ->
                   Size.( > ) contracts Size.zero
                 | Some (Notional notional) ->
                   Price.( > ) notional Price.zero
                 | None -> false
               in
               traded_today
               &&
               match venue with
               | Kalshi -> true
               | Polymarket -> Option.is_some l1.clob_token_id)
             |> (fun eligible -> List.take eligible 5)
             |> List.map ~f:L1_market_metadata.to_market_stub
           in
           (* Try markets until one book parses; thin mirrors serve null
              books for markets that still show listing quotes. *)
           let rec try_books = function
             | [] ->
               print_endline
                 [%string "%{venue#Venue}: no sample book obtained"];
               Deferred.unit
             | (stub : Market_stub.t) :: rest ->
               (match%bind.Deferred
                  Market_data.Market_data_gateway.fetch_book stub
                with
                | Ok book ->
                  print_s
                    [%message
                      "sample book"
                        ~market:(stub.title : string)
                        (book : Binary_book.t)];
                  Deferred.unit
                | Error error ->
                  print_s
                    [%message
                      "book unavailable"
                        ~market:(stub.title : string)
                        (error : Error.t)];
                  try_books rest)
           in
           try_books samples))
  in
  let%bind legs = Deferred.ok (Bot.fetch_legs candidates) in
  printf "books fetched for candidate markets: %d\n" (List.length legs);
  let scan detect_mode =
    Bot.scan
      ~execution:{ Config.Execution.default with detect_mode }
      ~legs
      candidates
  in
  print_s [%message "Exact" ~_:(scan Exact : Detect.opportunity list)];
  print_s [%message "Reckless" ~_:(scan Reckless : Detect.opportunity list)];
  return ()
;;

let () =
  don't_wait_for
    (let%bind.Deferred result = main () in
     (match result with
      | Ok () -> ()
      | Error error -> Core.eprint_s [%sexp (error : Error.t)]);
     shutdown 0;
     Deferred.unit);
  never_returns (Scheduler.go ())
;;
