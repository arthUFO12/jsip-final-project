open! Core
open! Async
open! Types
open Arbitrage

(* Reuses the stub/candidate builders from [Test_matcher]. Opening [Async]
   makes every expect test in this file async (bodies return [Deferred.t]),
   which the cache test needs; the pure tests just end with [return ()]. *)

let btc_kalshi =
  Test_matcher.stub
    ~id:"KXBTC-100K"
    ~venue:Venue.Kalshi
    ~close_time:(Time_ns.of_string_with_utc_offset "2026-12-31T23:00:00Z")
    "Will Bitcoin hit $100k in 2026?"
;;

let btc_poly =
  Test_matcher.stub
    ~id:"0x4f2a"
    ~venue:Venue.Polymarket
    "Bitcoin above $100,000 by December 31, 2026?"
;;

let candidate = Test_matcher.candidate

let%expect_test "cache_key depends on market ids, never titles" =
  let key =
    Llm_matcher.For_testing.cache_key (candidate btc_kalshi btc_poly)
  in
  let reworded = { btc_poly with title = "BTC >= $100K (2026)?" } in
  let key_after_rewording =
    Llm_matcher.For_testing.cache_key (candidate btc_kalshi reworded)
  in
  print_s [%sexp (key : Market_id.t * Market_id.t)];
  printf
    "same key after rewording: %b\n"
    ([%equal: Market_id.t * Market_id.t] key key_after_rewording);
  [%expect
    {|
    (KXBTC-100K 0x4f2a)
    same key after rewording: true
    |}];
  return ()
;;

let%expect_test "request_body has the Messages API shape" =
  print_endline
    (Yojson.Safe.pretty_to_string
       (Llm_matcher.For_testing.request_body (candidate btc_kalshi btc_poly)));
  [%expect
    {|
    {
      "model": "claude-haiku-4-5",
      "max_tokens": 1024,
      "system": "You decide whether two prediction markets settle on the same real-world event. Two markets match only if every outcome that resolves one YES would necessarily resolve the other YES: the same subject, the same measurement or source, the same numeric threshold, and the same deadline. Different thresholds (e.g. $100K vs $95K), different dates, or different resolution sources mean NOT a match, no matter how similar the titles read. Wording differences alone never matter. If the descriptions leave you genuinely unsure, answer that they do not match: a false match risks money, a missed match only misses an opportunity. Keep the explanation to one or two sentences naming the attribute that drove your decision.",
      "messages": [
        {
          "role": "user",
          "content": "Do these two prediction markets settle on the same real-world event?\nMarket A: venue: Kalshi; title: Will Bitcoin hit $100k in 2026?; closes: 2026-12-31 23:00:00.000000000Z\nMarket B: venue: Polymarket; title: Bitcoin above $100,000 by December 31, 2026?; closes: unknown"
        }
      ],
      "output_config": {
        "format": {
          "type": "json_schema",
          "schema": {
            "type": "object",
            "properties": {
              "is_match": { "type": "boolean" },
              "explanation": { "type": "string" }
            },
            "required": [ "is_match", "explanation" ],
            "additionalProperties": false
          }
        }
      }
    }
    |}];
  return ()
;;

let print_verdict response_body =
  print_s
    [%sexp
      (Llm_matcher.For_testing.parse_verdict response_body
       : Llm_matcher.verdict Or_error.t)]
;;

let%expect_test "parse_verdict reads a structured-outputs response" =
  print_verdict
    {|{"type":"message","stop_reason":"end_turn","content":[{"type":"text","text":"{\"is_match\": true, \"explanation\": \"Same subject, threshold, and deadline.\"}"}]}|};
  [%expect
    {| (Ok ((is_match true) (explanation "Same subject, threshold, and deadline."))) |}];
  return ()
;;

let%expect_test "parse_verdict rejects prose that is not verdict JSON" =
  (* A real response captured from curl -- valid API shape, but the text is
     chat, not a verdict. *)
  print_verdict
    {|{"type":"message","stop_reason":"end_turn","content":[{"type":"text","text":"Hi! How's it going?"}]}|};
  [%expect
    {|
    (Error
     ("could not parse model response"
      ("Yojson__Common.Json_error(\"Line 1, bytes 0-19:\\nInvalid token 'Hi! How's it going?'\")")))
    |}];
  return ()
;;

let%expect_test "parse_verdict rejects a truncated (max_tokens) response" =
  print_verdict
    {|{"type":"message","stop_reason":"max_tokens","content":[{"type":"text","text":"{\"is_match\": tru"}]}|};
  [%expect
    {|
    (Error
     ("could not parse model response"
      ("unexpected stop_reason" (stop_reason max_tokens))))
    |}];
  return ()
;;

let%expect_test "parse_verdict surfaces API error envelopes" =
  print_verdict
    {|{"type":"error","error":{"type":"authentication_error","message":"x-api-key header is required"}}|};
  [%expect
    {|
    (Error
     ("could not parse model response"
      ("API error" (error_message "x-api-key header is required"))))
    |}];
  return ()
;;

let%expect_test "adjudicate serves cache hits without touching the network" =
  let candidate = candidate btc_kalshi btc_poly in
  let key = Llm_matcher.For_testing.cache_key candidate in
  Hashtbl.set
    Llm_matcher.For_testing.cache
    ~key
    ~data:{ Llm_matcher.is_match = true; explanation = "seeded by test" };
  let%bind verdict = Llm_matcher.adjudicate candidate in
  print_s [%sexp (verdict : Llm_matcher.verdict Or_error.t)];
  Hashtbl.remove Llm_matcher.For_testing.cache key;
  [%expect {| (Ok ((is_match true) (explanation "seeded by test"))) |}];
  return ()
;;
