open! Core
open! Async
open! Types
open Arbitrage

(* Temporary probe: time each matching stage on a market snapshot so the slow
   stage is a number, not a guess. Manually run: dune exec
   lib/arbitrage/test/perf_probe.exe <snapshot> [kalshi-prefix] *)

let lap label stopwatch =
  let now = Time_ns.now () in
  Core.eprintf !"%s: %{Time_ns.Span}\n" label (Time_ns.diff now !stopwatch);
  Stdlib.flush Stdlib.stderr;
  stopwatch := now
;;

let main ~snapshot ~kalshi_prefix =
  let%bind.Deferred contents = Reader.file_contents snapshot in
  let kalshi, poly =
    [%of_sexp: Market_stub.t list * Market_stub.t list]
      (Sexp.of_string contents)
  in
  let kalshi =
    match kalshi_prefix with
    | None -> kalshi
    | Some prefix -> List.take kalshi prefix
  in
  Core.eprintf
    "markets: %d kalshi x %d poly\n"
    (List.length kalshi)
    (List.length poly);
  Stdlib.flush Stdlib.stderr;
  let stopwatch = ref (Time_ns.now ()) in
  let claims stubs = List.filter_map stubs ~f:Claim.of_stub in
  let kalshi_claims = claims kalshi in
  let poly_claims = claims poly in
  lap "compile claims" stopwatch;
  Core.eprintf
    "compiled: %d kalshi, %d poly\n"
    (List.length kalshi_claims)
    (List.length poly_claims);
  Stdlib.flush Stdlib.stderr;
  let blocked = Matcher.For_testing.block kalshi poly in
  lap "tag block" stopwatch;
  Core.eprintf "tag-blocked pairs: %d\n" (List.length blocked);
  Stdlib.flush Stdlib.stderr;
  let report =
    Matcher.compare_pipelines ~threshold:0.5 ~apply_veto:true kalshi poly
  in
  lap "compare_pipelines" stopwatch;
  Core.eprintf
    "judged: %d, near misses: %d, neither: %d\n"
    (List.length report.judged)
    (List.length report.near_misses)
    report.neither;
  Stdlib.flush Stdlib.stderr;
  Deferred.unit
;;

let () =
  let args = Sys.get_argv () in
  let snapshot = args.(1) in
  let kalshi_prefix =
    if Array.length args > 2 then Some (Int.of_string args.(2)) else None
  in
  don't_wait_for
    (let%bind.Deferred () = main ~snapshot ~kalshi_prefix in
     shutdown 0;
     Deferred.unit);
  never_returns (Scheduler.go ())
;;
