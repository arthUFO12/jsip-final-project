(** Per-field validation for the client's numeric text inputs, one function
    per field so each carries its own bounds and message. [Ok] holds the
    parsed value the request wants; [Error] holds the message rendered
    under the field ([(value, string) Result.t] rather than [Or_error.t] —
    these are display strings, not errors to propagate). Bounds mirror the
    server's so a request that validates here is never rejected there for
    range reasons. *)

open! Core

val lookback_days : string -> (int, string) Result.t
val warmup_hours : string -> (int, string) Result.t

(** The server also requires warmup strictly inside the lookback window;
    call with both fields' parsed values. [Some message] renders under the
    warmup field. *)
val warmup_within_lookback
  :  warmup_hours:int
  -> lookback_days:int
  -> string option

val starting_cash_dollars : string -> (int, string) Result.t

(** Dumb-bot comparison fields (only validated when the toggle is on). *)
val bot_count : string -> (int, string) Result.t

val trade_probability : string -> (float, string) Result.t
val bot_max_size : string -> (int, string) Result.t

(** Sweep and auto-review score thresholds share a 0..1 scale. *)
val match_threshold : string -> (float, string) Result.t

(** Arb backtest fields: assumed contracts per taken edge, and the entry
    threshold. *)
val arb_stake : string -> (int, string) Result.t

val min_edge_cents : string -> (int, string) Result.t

(** Hedge dialog step 2. *)
val fill_price_cents : string -> (int, string) Result.t

val fill_count : string -> (int, string) Result.t

(** The warning the rules editor shows before a definition whose name is
    already taken (a builtin, or an earlier definition) would be refused. *)
val definition_collision
  :  draft:string
  -> variables:string list
  -> string option
