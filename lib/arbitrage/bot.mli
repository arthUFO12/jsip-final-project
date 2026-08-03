open! Core
open! Async
open! Types

(** The polling loop gluing the pipeline together: read the approved pairs
    from the store (the gate {!Sweep} fills and a human curates), fetch each
    involved market's live order book, price with {!Detect} driven by
    {!Config.Execution}, and act on hits through an {!Execution.Executor}
    chosen once from {!Config.Trading} — [Paper] fills a
    {!Execution.Simulator} against the live book, [Live] sends real Kalshi
    orders (credentials from the environment; see
    {!Execution.Kalshi_live.Credentials}). Everything after that choice is
    the same code path. Orchestration and nothing else: no arbitrage
    arithmetic and no matching logic of its own.

    The caller must have called {!Database.Database_exec.init_database}
    before anything here runs — the store is where pairs come from. Pricing
    runs on real depth: {!fetch_legs} pulls live books
    ({!Market_data_gateway.fetch_book}), so {!Detect.Mode.Exact} sees true
    asks and true sizes. {!leg_of_l1} remains as a depthless approximation
    for offline screening. *)

(** [leg_of_l1 l1] approximates a leg from one listing record, or [None] when
    either side of the quote is missing. The NO ask is derived from the
    binary identity (NO ask = $1 - YES bid) and sizes are always {!Size.zero}
    — L1 metadata carries no depth — so these legs only surface hits under
    {!Detect.Mode.Reckless}. For real pricing use {!fetch_legs}. *)
val leg_of_l1 : L1_market_metadata.t -> Detect.Leg.t option

(** [fetch_legs candidates] fetches the live order book for every distinct
    market the candidates touch and returns the priceable legs. Markets whose
    book fetch fails (network, empty side, missing token id) are skipped with
    a note on stderr, never fatal. *)
val fetch_legs : Matcher.Candidate.t list -> Detect.Leg.t list Deferred.t

(** [confirm_pairs ~matching lefts rights] runs the text pipeline at the
    thresholds [matching] dictates, then — in LLM mode — keeps only
    candidates the adjudicator confirms. Adjudication errors skip the pair
    with a note on stderr, never fatal. *)
val confirm_pairs
  :  matching:Config.Matching.t
  -> Market_stub.t list
  -> Market_stub.t list
  -> Matcher.Candidate.t list Deferred.t

(** The approved pairs from the store, joined back to their market stubs.
    What the bot prices every tick. Pairs with missing stubs are skipped with
    a note on stderr. *)
val candidates_of_approved
  :  unit
  -> Matcher.Candidate.t list Deferred.Or_error.t

(** [scan ~execution ~legs candidates] prices every confirmed candidate whose
    markets appear in [legs] and returns the hits: opportunities at least
    [execution.min_edge] thick, sizes capped at
    [execution.stake_per_opportunity]. Pure — all I/O happened upstream in
    {!fetch_legs}. *)
val scan
  :  execution:Config.Execution.t
  -> legs:Detect.Leg.t list
  -> Matcher.Candidate.t list
  -> Detect.opportunity list

(** [scan_once ~config] is one full tick of detection only: read the approved
    pairs, fetch their books, price. Individual book failures only drop their
    market. No orders are placed — {!run} is the loop that acts. *)
val scan_once
  :  config:Config.t
  -> Detect.opportunity list Deferred.Or_error.t

(** [orders_of_opportunity ~stubs opportunity] is the pair of orders acting
    on a hit: buy YES on one venue and NO on the other, each limited at the
    ask the detector saw and sized at the opportunity's size. [stubs] joins
    the opportunity's market ids back to the routing data an order carries; a
    missing stub is an error. Pure. *)
val orders_of_opportunity
  :  stubs:Market_stub.t Market_id.Map.t
  -> Detect.opportunity
  -> Execution.Order.t list Or_error.t

(** [run ~config] validates [config], builds the executor its
    {!Config.Trading} names — this is the only paper-vs-live fork — and loops
    every [poll_interval]: scan, then place both legs of each hit, printing
    fills. Quiet ticks print a heartbeat instead — how many pairs were priced
    and the best edge that fell short of [min_edge] — so a running bot is
    always visibly alive. Returns promptly with an error on an invalid config
    or when [Live] credentials are missing from the environment; otherwise
    the result stays undetermined for the life of the bot — scan and order
    failures are logged and retried, never fatal. *)
val run : config:Config.t -> unit Deferred.Or_error.t
