open! Core
open! Types

module Matching = struct
  type t =
    | Llm_assisted of { threshold : float }
    | Text_only of { threshold : float }
  [@@deriving sexp_of]

  (* In LLM mode the threshold only sheds obvious junk before adjudication,
     so it sits just above zero; in text-only mode it is the final word on
     whether two markets match, so it sits much higher. *)
  let default_llm_assisted = Llm_assisted { threshold = 0.1 }
  let default_text_only = Text_only { threshold = 0.5 }

  let threshold = function
    | Llm_assisted { threshold } | Text_only { threshold } -> threshold
  ;;

  (* The veto is unrecoverable — a vetoed pair is invisible to every later
     stage — so it only runs when nothing smarter comes after it. *)
  let apply_veto = function Llm_assisted _ -> false | Text_only _ -> true
  let use_llm = function Llm_assisted _ -> true | Text_only _ -> false
end

module Trading = struct
  type t =
    | Paper
    | Live
  [@@deriving sexp_of]

  (* Nothing should be able to reach Live by accident, so the default matters
     more than the type. *)
  let default = Paper
end

module Execution = struct
  type t =
    { poll_interval : Time_ns.Span.t
    ; stake_per_opportunity : Size.t
    ; min_edge : Price.t
    ; detect_mode : Detect.Mode.t
    ; max_dollars_per_order : Price.t
    ; max_dollars_per_day : Price.t
    }
  [@@deriving sexp_of]

  (* The caps default small on purpose: a fresh live config can lose at most
     pocket money until someone deliberately raises them. *)
  let default =
    { poll_interval = Time_ns.Span.of_sec 1.
    ; stake_per_opportunity = Size.of_int 10
    ; min_edge = Price.of_int_cents 1
    ; detect_mode = Exact
    ; max_dollars_per_order = Price.of_int_cents 2_500
    ; max_dollars_per_day = Price.of_int_cents 10_000
    }
  ;;
end

type t =
  { matching : Matching.t
  ; trading : Trading.t
  ; execution : Execution.t
  }
[@@deriving sexp_of]

let default =
  { matching = Matching.default_llm_assisted
  ; trading = Trading.default
  ; execution = Execution.default
  }
;;

let validate t =
  let { matching = _
      ; trading
      ; execution =
          { poll_interval
          ; stake_per_opportunity
          ; min_edge
          ; detect_mode
          ; max_dollars_per_order
          ; max_dollars_per_day
          }
      }
    =
    t
  in
  let check ok error = if ok then Ok () else Or_error.error_s error in
  let live = match trading with Live -> true | Paper -> false in
  let checks =
    [ check
        (Time_ns.Span.( > ) poll_interval Time_ns.Span.zero)
        [%message
          "poll_interval must be positive" (poll_interval : Time_ns.Span.t)]
    ; check
        (Size.( > ) stake_per_opportunity Size.zero)
        [%message
          "stake_per_opportunity must be positive"
            (stake_per_opportunity : Size.t)]
    ; check
        (Price.( >= ) min_edge Price.zero)
        [%message "min_edge must not be negative" (min_edge : Price.t)]
    ; check
        (match trading, detect_mode with
         | Live, Reckless -> false
         | (Paper | Live), (Exact | Reckless) -> true)
        [%message
          "Reckless detection may only feed Paper trading, never Live"]
    ; (* Real money must run inside hard caps: a live config with a disabled
         cap is invalid, not merely inadvisable. *)
      check
        ((not live) || Price.( > ) max_dollars_per_order Price.zero)
        [%message
          "Live trading requires a positive max_dollars_per_order"
            (max_dollars_per_order : Price.t)]
    ; check
        ((not live) || Price.( > ) max_dollars_per_day Price.zero)
        [%message
          "Live trading requires a positive max_dollars_per_day"
            (max_dollars_per_day : Price.t)]
    ]
  in
  Or_error.map (Or_error.combine_errors_unit checks) ~f:(fun () -> t)
;;
