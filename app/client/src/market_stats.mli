(** Pure statistics for the market-detail popup, computed client-side from
    the wire series in {!Protocol.Market_detail}. Points are [(time, value)]
    pairs in ascending time order, times in epoch seconds — the same shape
    the popup's charts plot. *)

open! Core

(** Each input point paired with the sum of every value whose time lies in
    [(t - window_s, t]] — e.g. [~window_s:86400.] turns per-candle volumes
    into a trailing 24-hour volume line. Input order is preserved. *)
val trailing_sum
  :  window_s:float
  -> (float * float) list
  -> (float * float) list

(** Standard deviation of the successive differences of [values] — how jumpy
    the series is, insensitive to its absolute level. [0.] when there are
    fewer than three values (no two differences to spread). *)
val volatility : float list -> float
