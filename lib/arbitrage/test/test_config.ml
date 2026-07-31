open! Core
open! Types
open Arbitrage

let print_validated config =
  print_s [%sexp (Config.validate config : Config.t Or_error.t)]
;;

let%expect_test "the default config is valid by construction" =
  print_validated Config.default;
  [%expect
    {|
    (Ok
     ((matching (Llm_assisted (threshold 0.1))) (trading Paper)
      (execution
       ((poll_interval 1s) (stake_per_opportunity 10) (min_edge 1000000)
        (detect_mode Exact)))))
    |}]
;;

let%expect_test "validate reports every offending knob at once" =
  let broken =
    { Config.default with
      execution =
        { poll_interval = Time_ns.Span.zero
        ; stake_per_opportunity = Size.zero
        ; min_edge = Price.neg (Price.of_int_cents 1)
        ; detect_mode = Exact
        }
    }
  in
  print_validated broken;
  [%expect
    {|
    (Error
     (("poll_interval must be positive" (poll_interval 0s))
      ("stake_per_opportunity must be positive" (stake_per_opportunity 0))
      ("min_edge must not be negative" (min_edge -1000000))))
    |}]
;;

let%expect_test "Reckless detection may not feed Live trading" =
  let reckless execution =
    { execution with Config.Execution.detect_mode = Detect.Mode.Reckless }
  in
  (* Paper + Reckless is allowed -- that's what the mode is for. *)
  print_validated
    { Config.default with
      trading = Paper
    ; execution = reckless Config.Execution.default
    };
  (* Live + Reckless is the combination validate exists to stop. *)
  print_validated
    { Config.default with
      trading = Live
    ; execution = reckless Config.Execution.default
    };
  [%expect
    {|
    (Ok
     ((matching (Llm_assisted (threshold 0.1))) (trading Paper)
      (execution
       ((poll_interval 1s) (stake_per_opportunity 10) (min_edge 1000000)
        (detect_mode Reckless)))))
    (Error "Reckless detection may only feed Paper trading, never Live")
    |}]
;;
