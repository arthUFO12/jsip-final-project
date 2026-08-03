open! Core
open! Async
open! Types

(** The discovery half of the pipeline: find likely cross-venue pairs and
    file them with the review gate. For each of the most-traded Kalshi
    markets, searches Polymarket for similar markets (the venue's search
    engine is the first funnel), runs the hits through the text matcher and —
    per {!Config.Matching} — the LLM, and writes surviving pairs to the pair
    store as proposals, alongside their stubs. Pairs the store already knows
    (any status) are skipped {e before} any LLM spend, each market sends only
    its best-scoring candidates to the LLM, and LLM rejections are themselves
    recorded — so sweeps are cheap to repeat and can never resurrect a
    rejected pair. The LLM never approves: confirmed pairs land as [Proposed]
    for a human to judge.

    The caller must have called {!Database.Database_exec.init_database}.
    Search recall belongs to the venue: a market missed here is invisible, so
    an occasional full-listing sweep is still the recall backstop. *)

type summary =
  { markets_swept : int (** Kalshi markets we searched from *)
  ; search_hits : int (** Polymarket markets the searches returned *)
  ; fresh_candidates : int
  (** candidates surviving the text matcher and not already in the store,
      capped at the per-market limit *)
  ; proposed : int (** pairs written to the store as [Proposed] *)
  ; auto_rejected : int
  (** pairs the LLM rejected, recorded as [Rejected] with the explanation so
      no future sweep pays to ask again *)
  }
[@@deriving sexp_of]

(** [sweep_once ~matching ~markets_to_sweep ()] runs one sweep over the
    [markets_to_sweep] highest-volume Kalshi markets. Individual search
    failures skip that market with a note on stderr; only the initial listing
    fetch is fatal. *)
val sweep_once
  :  matching:Config.Matching.t
  -> markets_to_sweep:int
  -> unit
  -> summary Deferred.Or_error.t

(** [compare_once ~threshold ~apply_veto ~markets_to_sweep ()] runs the same
    discovery as {!sweep_once} — the top Kalshi markets by volume, one
    Polymarket search each — but instead of filing proposals it returns both
    matching systems' verdicts on every discovered pair, via
    {!Matcher.compare_pipelines}. Read-only: nothing is written to the store
    and no LLM is called, so it is safe to run as often as needed while
    evaluating the matchers against each other. *)
val compare_once
  :  threshold:float
  -> apply_veto:bool
  -> markets_to_sweep:int
  -> unit
  -> Matcher.Comparison.t list Deferred.Or_error.t

(** [sweep_full ~matching ~max_adjudications ()] is the recall backstop: page
    through {e every} open market on both venues and run the matcher over the
    full cross product, filing survivors exactly as {!sweep_once} does. Our
    matcher's recall, not the venue's search engine's — anything listed on
    both venues is eventually compared. Slow and meant to run occasionally.
    In LLM mode at most [max_adjudications] fresh pairs are adjudicated per
    run (best scores first, the rest noted on stderr and left for next time);
    text-only mode has no budget to spend. *)
val sweep_full
  :  matching:Config.Matching.t
  -> max_adjudications:int
  -> unit
  -> summary Deferred.Or_error.t
