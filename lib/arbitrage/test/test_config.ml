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
        (detect_mode Exact) (max_dollars_per_order 2500000000)
        (max_dollars_per_day 10000000000)))))
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
        ; max_dollars_per_order =
            Config.Execution.default.max_dollars_per_order
        ; max_dollars_per_day = Config.Execution.default.max_dollars_per_day
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
        (detect_mode Reckless) (max_dollars_per_order 2500000000)
        (max_dollars_per_day 10000000000)))))
    (Error "Reckless detection may only feed Paper trading, never Live")
    |}]
;;

let%expect_test "Live trading refuses to run without positive spending caps" =
  let uncapped =
    { Config.Execution.default with
      max_dollars_per_order = Price.zero
    ; max_dollars_per_day = Price.zero
    }
  in
  (* Paper doesn't care: nothing real is being capped. *)
  print_validated
    { Config.default with trading = Paper; execution = uncapped };
  (* Live must be inside hard caps — a disabled cap is invalid, not merely
     inadvisable. *)
  print_validated
    { Config.default with trading = Live; execution = uncapped };
  [%expect
    {|
    (Ok
     ((matching (Llm_assisted (threshold 0.1))) (trading Paper)
      (execution
       ((poll_interval 1s) (stake_per_opportunity 10) (min_edge 1000000)
        (detect_mode Exact) (max_dollars_per_order 0) (max_dollars_per_day 0)))))
    (Error
     (("Live trading requires a positive max_dollars_per_order"
       (max_dollars_per_order 0))
      ("Live trading requires a positive max_dollars_per_day"
       (max_dollars_per_day 0))))
    |}]
;;
