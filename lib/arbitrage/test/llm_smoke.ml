open! Core
open! Async
open! Types
open Arbitrage

(* Live smoke test for the LLM matcher: costs a fraction of a cent per run
   and needs ANTHROPIC_API_KEY in the environment. Deliberately not an expect
   test -- it hits the network and its output depends on the model.

   Run with: dune exec lib/arbitrage/test/llm_smoke.exe *)

let stub ?close_time ~venue ~id title =
  { Market_stub.venue
  ; market_id = Market_id.of_string id
  ; slug = Slug.of_string id
  ; series_ticker = None
  ; clob_token_id = None
  ; title
  ; category = Miscellaneous
  ; close_time
  }
;;

let close_time = Time_ns.of_string_with_utc_offset "2026-12-31T23:00:00Z"

let should_match =
  { Matcher.Candidate.left =
      stub
        ~venue:Venue.Kalshi
        ~id:"KXBTC-100K"
        ~close_time
        "Will Bitcoin reach $100,000 by December 31, 2026?"
  ; right =
      stub
        ~venue:Venue.Polymarket
        ~id:"0xbtc100k"
        ~close_time
        "Bitcoin above $100,000 on Dec 31, 2026?"
  }
;;

let should_not_match =
  { Matcher.Candidate.left =
      stub
        ~venue:Venue.Kalshi
        ~id:"KXBTC-100K"
        ~close_time
        "Will Bitcoin reach $100,000 by December 31, 2026?"
  ; right =
      stub
        ~venue:Venue.Polymarket
        ~id:"0xbtc95k"
        ~close_time
        "Bitcoin above $95,000 on Dec 31, 2026?"
  }
;;

let main () =
  Deferred.List.iter
    ~how:`Sequential
    [ "should match", should_match; "should NOT match", should_not_match ]
    ~f:(fun (expectation, candidate) ->
      let%map verdict = Llm_matcher.adjudicate candidate in
      print_s
        [%message expectation (verdict : Llm_matcher.verdict Or_error.t)])
;;

let () =
  don't_wait_for
    (let%bind () = main () in
     shutdown 0;
     return ());
  never_returns (Scheduler.go ())
;;
