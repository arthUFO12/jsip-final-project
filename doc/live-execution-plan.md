# Plan: real order execution from the web app (Kalshi + Polymarket)

Status: IN PROGRESS — reviewed 2026-08-04 by a second Claude (findings
folded in), then the no-real-money slice was green-lit and built the
same day. Done: milestone 0.5 (legging abort + regression test), Phase 0
(trade_log audit table, spending caps in config + validate, kill switch,
explicit `-live` gate on the CLI run verb), and Phase 1's plumbing
(caller-supplied `client_order_id`, host threading with demo default in
`check-trade`, cancel + order-status endpoints, and the `check-trade`
demo round-trip verb). Same-day discovery while running `check-trade` against demo: Kalshi has
deprecated the old create-order endpoint (410 `deprecated_v1_order_endpoint`)
in favor of a V2 surface — `POST /trade-api/v2/portfolio/events/orders`,
single-book `bid`/`ask` sides (a NO order maps through the binary
identity: buy NO at q = ask YES at $1−q), fixed-point dollar strings,
required `time_in_force`/`self_trade_prevention_type` — `kalshi_live.ml`
now speaks it, with the encoding and response parsing pinned by expect
tests. **Milestone 1 COMPLETE (2026-08-04):** with the demo account funded,
`check-trade` ran the full lifecycle — placed a 1-contract 1¢ resting
order, read `resting`, canceled (`reduced_by 1.00`), all in the audit
log; the demo balance came through untouched. Venue behaviors learned
and encoded: GET-by-id lives only at the legacy `/portfolio/orders/{id}`
path (V2 404s), and the read side lags the matching engine by a couple
of seconds (a just-placed order reads `not_found`; `check-trade`
retries). One cosmetic quirk: status can still read `resting` right
after a successful cancel, for the same lag reason. **Milestone 2 COMPLETE (2026-08-04):** `check-trade -real` (an explicit
production flag with a worst-case banner; credentials passed inline so
`.env` stays demo-defaulted) ran the same lifecycle on production
Kalshi — 1 contract, YES bid at 1¢ on a far-dated quiet market —
placed, rested, canceled (`reduced_by 1`), **$0.00 spent**. The audit
log records it as venue `Kalshi` with its $0.01 notional, alongside the
demo history. Every demo-learned venue behavior (read lag, legacy
status path, fixed-point cancel) held on production. Operator follow-up:
rotate the production API key (it transited a chat). Remaining gate for
Phase 2: the manual Polymarket eligibility test order.

## Goal

Let the app place the two legs of an approved, detected arbitrage on the
user's real Kalshi and Polymarket accounts — from the Arb Pairs page —
with the paper wallet's "acted" column fed by real fills instead of an
honor-system button.

## What already exists (do not rebuild)

The codebase was structured for this from the start; the plan is mostly
*finishing* the live path, not creating one.

| Piece | Where | State |
| --- | --- | --- |
| Paper/live fork | `lib/execution/executor.mli` — `Executor.t`, built once from config; strategies never branch on mode again | Done |
| Venue-agnostic order | `lib/execution/order.mli` — carries the whole `Market_stub.t` so routing data (Kalshi ticker, Polymarket `clob_token_id`) travels with the order | Done |
| Kalshi live placement | `lib/execution/kalshi_live.ml` — RSA-PSS(`timestamp ^ verb ^ path`) signing, POST to `/trade-api/v2/portfolio/orders`, credentials from `KALSHI_API_KEY_ID` + `KALSHI_PRIVATE_KEY_FILE`, signing covered by `For_testing` verify tests | Done, but fire-and-forget (see gaps) |
| Polymarket live | `Executor` **rejects by design** — its CLOB needs an EIP-712 wallet signature we don't produce | Missing (the big lift) |
| Order construction | `Bot.orders_of_opportunity` — YES leg on one venue, NO on the other, at the asks the detector priced | Done |
| Legging discipline | `Bot.execute` — legs sent sequentially; first-leg failure aborts the rest; one-sided outcome is loud | Done — **fixed 2026-08-04**: review caught that the original `Deferred.List.iter` swallowed each leg's error and proceeded to leg 2 while the comment claimed abort; now an explicit early-stop recursion (no unwind yet) |
| Fees | `Execution.Fees.taker_fee`, used by both `Detect` and fills | Done (Polymarket schedule unverified — see open questions) |
| Sim parity | `Simulator` with `Against_live_book` fill model — paper fills face real depth and fees | Done |
| Web scan surface | `Arb_runner.scan` + wallet booking + `mark_acted` | Done (paper only) |

## Gap analysis

1. **Polymarket cannot trade at all.** Its CLOB requires EIP-712-signed
   orders from a Polygon wallet holding USDC, plus L2 API credentials
   derived from that wallet. Without it, "live" arbitrage is one-sided —
   which is not arbitrage.
2. **Kalshi path is fire-and-forget.** A `client_order_id` exists
   (`kalshi_live.ml` mints `arbiter-<timestamp_ms>-<counter>` inside
   `place_order`) but is fresh per call, so a retry after a timeout gets
   a new id and can double-place — the id must become caller-supplied
   (stable per attempt) to be idempotent. No cancel, no tracking of a
   resting remainder, no fill/position reconciliation (`kalshi_live.mli`
   names this as future work). The venue host is a hardcoded constant
   (`api.elections.kalshi.com`, `kalshi_live.ml:5`), so the demo
   environment is unreachable without threading the host through
   credentials/config.
3. **No server-side execution surface.** The web app only scans; nothing
   authorizes or routes a real order from the browser, and nothing must
   ever put venue credentials in the browser.
4. **No risk rails.** No per-order/day spending caps, no kill switch, no
   audit trail, no unwind policy for a stranded first leg.

## Proposed architecture

```
browser (Bonsai)                      server (app/server)                 venues
┌─────────────────┐  execute-edge RPC  ┌──────────────────┐
│ Arb Pairs page  │ ─────────────────▶ │ Trade_runner      │ ──▶ Kalshi_live (OCaml, exists)
│ per-order       │   pair_key + cap   │  - risk checks    │
│ confirm dialog  │ ◀───────────────── │  - audit log      │ ──▶ Polymarket sidecar (localhost only)
└─────────────────┘   fills / refusal  │  - wallet update  │        └─ official py-clob-client
                                       └──────────────────┘
```

Principles:

- **Credentials live only in the server process environment** (`.env`,
  same pattern as `ANTHROPIC_API_KEY` / `KALSHI_*` today). They are never
  stored in sqlite, never serialized into `Protocol`, never sent to or
  from the browser. `lib/protocol` and `lib/types` stay js_of_ocaml-clean
  — no crypto dependencies leak into client-linked libraries.
- **The browser only ever authorizes; the server executes.** The RPC
  request is "execute this pair_key with at most $X" — never prices,
  keys, or raw orders from the client.
- **Every real order goes through `Executor.t`.** The webapp gets a live
  executor the same way the CLI bot does; no second money-moving path.

## Credentials: the bring-your-own posture

The app runs on the user's own machine, so "encrypted and secure" means
the ssh model, honestly applied — not security theater:

- **Keys live in the user's home directory** (`~/.kalshi/*.pem`),
  never inside the repo (`.env` is gitignored; key files never enter the
  tree at all), never in sqlite, never in `Protocol` types, and never in
  the browser: the web UI has no field for venue keys and no RPC that
  could carry one. Users plug in their own by editing `.env` locally.
- **Loose permissions are refused, not warned about**:
  `Credentials.load_from_env` stats the key file and errors on any
  group/other read bits, with the `chmod 600` fix in the message — same
  contract as sshd.
- **Demo is the default**: `.env` carries the demo pair active and the
  production pair commented out, so a routine restart cannot pick up the
  live key by accident; production requires editing `.env` *and* the
  `-live` flag *and* passing `validate`'s caps.
- **Encryption at rest is deliberately deferred**: a passphrase-encrypted
  PEM (or OS keychain) would demand interactive entry on every unattended
  bot start, which fights the running-a-bot use case. If wanted later,
  encrypted-PKCS#8 support in `Credentials.create` is the extension
  point. On a single-user machine, 0600-enforced files match the posture
  of `~/.ssh` and `~/.aws/credentials`.
- **Chat is not a channel**: any key that transits a conversation
  (including this project's own history) should be rotated once its
  testing purpose is served.

## Phases

### Phase 0 — Safety rails (before any new live code)

- `trade_log` sqlite table: every attempt, order, venue response, and
  refusal, timestamped — append-only audit trail.
- `Config.Execution` gains hard caps: `max_dollars_per_order`,
  `max_dollars_per_day`, `max_open_orders`; `validate` rejects a live
  config that omits them.
- Kill switch: a `TRADING_DISABLED` env/flag checked by `Trade_runner`
  before every order. **One-way from the browser**: an RPC can trip it,
  but clearing it requires server-side action (restart or local
  command) — re-enabling trading from a browser would weaken the
  "browser only authorizes, server executes" principle.
- Server refuses to build a live executor unless explicitly started with
  `-allow-live` (a launch flag, so a routine restart can't go live by
  accident).

### Phase 1 — Finish the Kalshi leg (smallest real-money milestone)

- Make `client_order_id` caller-supplied (stable per attempt) instead of
  minted fresh inside `place_order`, so a retry after a timeout re-sends
  the same id and cannot double-place.
- Develop against **Kalshi's demo environment** (`demo-api.kalshi.co`),
  which requires threading the venue host through `Credentials`/config
  instead of the hardcoded `api.elections.kalshi.com` — one task,
  pairing with Phase 0's `-allow-live` flag: demo host unless
  explicitly live. The integration test places and cancels a
  1-contract order there.
- Add cancel (`DELETE /portfolio/orders/{id}`) and order-status /
  fills polling; reconcile the resting remainder the current code drops.
- Definition of done: `check-trade` headless subcommand round-trips
  place → poll → cancel on demo, and the audit log shows it.

### Eligibility finding (2026-08-04) and execution modes

The manual test settled open question 1: Polymarket's app works from the
US for browsing, but **US persons cannot trade there** — the venue is
barred from serving them. That splits execution into three modes, chosen
per account, and reorders Phase 2:

- **Assisted (the US mode; near-term).** The app cannot and does not
  place the Polymarket leg. Instead, for a detected edge, the user
  places whatever they can themselves; the app then fires the Kalshi
  leg — which it fully automates as of milestone 2 — inside the rails.
  Ordering preserves the legging principle with a human as leg 1: the
  user confirms their manual leg (venue, price, size) first, and only
  then does the app hedge on Kalshi. No confirmation, no Kalshi order.
  This needs zero new venue integration and is the next build item.
- **Auto (eligible non-US accounts; deferred).** The Phase 2 sidecar as
  originally planned, for accounts the venue itself accepts. Deferred
  until such an account actually exists to test with — building
  wallet-signing infrastructure with no eligible account to run it is
  spend without verification.
- **Watch-only.** Scan and wallet, no execution — what exists today.

Explicitly out of scope in all modes: VPN integration or any feature
whose purpose is defeating the venue's geoblocking. Eligibility is
between the account holder and the venue; the app takes the venue's
answer (`Geo_blocked` stays sticky) and never helps manufacture a
different one.

### Phase 3a — Assisted execution (design REVIEWED 2026-08-04; findings folded in)

The human places the leg the app can't; the app instantly hedges the leg
it has already proven it can. Kalshi execution is milestone-2-proven, so
everything new here is orchestration and UI — no new venue code.

**Who this is actually for** (review finding 1): eligible users who can
trade Polymarket but lack API/wallet setup — a stopgap until Phase 2's
auto mode — and, later, any manual venue. It is {e not} a US mode: a US
account has no legitimate Polymarket fill to confirm, so for such
accounts the assisted button is gated off by the sticky venue-wide
`Geo_blocked` flag once that flag exists (until then, the dialog's
step-1 warning carries the eligibility notice). The flow takes the
user's word and assists with nothing venue-side; it must never become a
ledger for fills obtained around a geoblock.

**Flow (two-step confirm, human is leg 1):**

1. A scan shows a tradable edge. On a live-enabled server the card
   offers **"Hedge assisted…"**, opening step 1 of a dialog: the manual
   leg's instructions — venue chip, title, the venue link, target price
   (the detected ask) and size — plus the trust warning: {e the app
   cannot place or verify this leg; everything you enter next is on
   your say-so}. Step 1 also runs a **pre-flight** (review finding 2):
   the same rails checked advisorily — kill switch state, caps room,
   current Kalshi ask and depth — rendered as "hedgeable up to N
   contracts right now", so the user sizes their manual leg to what the
   app can actually hedge instead of discovering the ceiling after
   they've committed money. The authoritative check still runs at
   execution; pre-flight narrows the race, it cannot close it.
2. The user trades (or fails to, or half-fills) on the venue themselves,
   then enters what they actually got: filled price (cents) and filled
   count. "I didn't trade" cancels — nothing happens at all.
3. The app fires the Kalshi hedge sized to the {e confirmed} manual
   count (never the detected size — hedge what the user actually
   holds), at a limit no worse than the max shown in the dialog. Rails
   run first: kill switch, per-order and per-day caps, and a slippage
   guard — the hedge re-reads the live Kalshi book and {e refuses}
   (rather than pays up) if the ask has moved past the shown limit.
4. Result rendering is three-armed, per the Phase 3 response design:
   `Hedged` (fill summary; wallet entry marked acted with the {e real}
   combined numbers, replacing the honor-system button), `Refused`
   (typed reason; the user is told they are one-sided {e by their own
   manual leg} and the hedge can be retried), or `Failed` (venue error
   after send — same one-sided alert, loud and persistent).

**Wire additions (all additive):**

- `Execution_capability.t = { live : bool; reason : string option }` —
  served so the UI knows whether to render assisted buttons at all, and
  {e why not} when it shouldn't (paper server vs. kill switch tripped).
- `Hedge_request.t = { pair_key; nonce; manual_venue; manual_is_yes;
  manual_price_cents; manual_count; max_hedge_price_cents }` — the max
  is what the dialog displayed, making the slippage guard the user's
  own number; `manual_is_yes` says which side of the split the user
  took, so the server hedges the opposite contract; the `nonce` is
  minted at dialog-confirm time and derives the Kalshi
  `client_order_id` (review finding 4), so re-sending the same
  confirmation after a timeout cannot double-hedge — exactly what
  Phase 1's caller-supplied id exists for.
- `Venue_error.t = Geo_blocked | Insufficient_funds |
  Market_closed_or_halted | Rate_limited | Other of string` — the
  Phase 3 taxonomy, introduced here (review finding 3: errors are
  first-class, not strings).
- `Hedge_result.t = Hedged of { price; count; fee; unhedged : int } |
  Refused of reason | Failed of { error : Venue_error.t; detail :
  string; unhedged : int }` — the unhedged count rides the wire in both
  the partial-fill and failure arms, so "still one-sided by N" is data,
  not prose.
- `preflight_hedge : pair_key -> Preflight.t` — the advisory numbers
  step 1 renders.

**Server changes:**

- The server gains its first live path: a `-allow-live` launch flag (per
  Phase 0's design) plus `KALSHI_*` env credentials build one live
  `Executor.t` at startup; absent either, `Execution_capability.live =
  false` and the hedge RPC refuses with a typed reason.
- The rails move from `Bot`-internal to shared: `live_refusal` and
  `log_live_order` factor into one place both `Bot.execute` and the new
  hedge path call, so the CLI bot and the web server cannot drift on
  what "inside the rails" means.
- The hedge order is rebuilt server-side from the pair's stubs (never
  trusted from the client): which side of the pair is the Kalshi leg,
  YES or NO per the detected split, limit = min(current ask,
  max_hedge_price_cents), size = manual count capped by book depth and
  the caps. Partial hedge fills are reported as such — the user hears
  "you are still one-sided by N contracts", not a rounded-up success.

**Audit and wallet:**

- Two trade-log rows per assisted execution: the user's manual leg
  (action `manual`, venue e.g. `Polymarket(manual)`, dollars as
  reported — clearly labeled unverified) and the Kalshi placement
  through the standard live logging. The audit trail shows the whole
  pair, with the trust boundary visible in the venue label.
- The wallet's acted column takes real numbers: manual dollars (as
  reported) + actual Kalshi fill, replacing the honor-system flow for
  assisted pairs.

**Out of scope for 3a:** auto-anything on Polymarket; retrying the
manual leg; verifying the manual leg against Polymarket's public data
(worth a later "sanity-check against the book" nicety); any use of the
manual flow for venues the app {e can} trade (Kalshi legs are always
automated).

**Definition of done:** on a `-allow-live` server, one real assisted
execution end to end — human leg confirmed, Kalshi hedge filled inside
the rails, both rows in the audit log, wallet showing real dollars — and
expect tests for hedge-order construction (side mapping, sizing rule,
slippage refusal) plus a demo-host `check-hedge` CLI verb mirroring
`check-trade`.

**Open questions — answered in review:**

1. `min(current ask, max)`, no re-confirm on improvement: a cheaper
   hedge is strictly better and re-confirmation is friction during
   which the improved ask can vanish.
2. Yes — assisted entries are visibly marked **self-reported** in the
   wallet; the audit label alone never reaches the scoreboard, and
   unlabeled say-so dollars would recreate the honor-system problem.
3. Additive factoring (new rails module) needs only a courtesy
   heads-up to Arthur; actual coordination is reserved for mutations to
   surfaces he already links.

### Phase 2 — Polymarket live (deferred pending an eligible account)

Two viable routes, in order of recommendation:

- **A. Sidecar using the official `py-clob-client` (recommended first).**
  A small Python process on localhost owns the Polygon wallet key,
  derives L2 API creds, signs EIP-712 orders, and exposes a minimal
  local-only HTTP interface (`place`, `cancel`, `status`) that a new
  `Polymarket_live` OCaml module calls. Pros: battle-tested signing and
  auth, small OCaml surface, key isolated in one process. Cons: a second
  runtime to deploy.
- **B. Native OCaml EIP-712** (keccak-256 via `digestif`, secp256k1
  bindings, typed-data encoding). Pros: one binary, educational. Cons:
  hand-rolled crypto in the money path — highest-risk code we could
  write; recommend only after A works and only with test vectors from
  the official client.

Either route also needs one-time wallet setup (USDC on Polygon, CLOB
allowance approvals) — documented as an operator runbook, not code.
`Executor.live` grows from Kalshi-credentials-only to a record holding
both venues' credentials, each optional; an order routed to a venue
without credentials fails loudly *before* the other leg is sent.

### Phase 3 — Web surface

- `execute_edge` RPC: request `{ pair_key; max_dollars }`. The response
  type has **three arms**, because money can move without success:
  `Filled` (both legs), `Refused` (typed: cap exceeded, trading
  disabled, stale edge — re-price before sending, refuse if the edge is
  gone), and `One_sided` (leg 1 filled, leg 2 failed — a real position
  exists; the UI must render it as an open risk needing attention, not
  as a generic failure). Phase 4 owns what to *do* about one-sided;
  Phase 3's wire type must already represent it.
- **Venue errors are first-class, not strings.** Execution failures come
  back as a typed taxonomy, because different failures demand different
  UI: `Geo_blocked` (US-based users cannot trade certain — possibly all —
  Polymarket markets; the CLOB rejects by region/eligibility),
  `Insufficient_funds`, `Market_closed_or_halted`, `Rate_limited`, and
  `Venue_error of string` as the catch-all. Every one lands in
  `trade_log`.
- **Geo discovery must not cost a Kalshi leg.** If eligibility were
  first discovered at execution time, the discovery order matters: with
  today's YES-venue-first leg ordering, a geo-blocked Polymarket leg 2
  would arrive *after* the Kalshi leg filled — a `One_sided` outcome
  that cost real money just to learn "no". Two mitigations, both
  adopted: (1) **deliberate leg ordering** — send the
  eligibility-uncertain venue (Polymarket) first, so its rejection
  costs nothing and Kalshi, already demo-proven, is leg 2; today
  `orders_of_opportunity` orders YES-venue-first regardless of venue,
  so this is a small explicit policy change in Phase 3. (2) **Detect
  before trading** — venue-wide ineligibility generally surfaces when
  deriving Polymarket's L2 API credentials (and on the venue's own UI),
  so Phase 2's operator runbook plus open question 1's manual test
  order establish venue-level eligibility before any OCaml code sends
  an order; the sticky flag below then only ever handles *per-market*
  restrictions.
- **`Geo_blocked` is sticky.** When a venue rejects a market for
  eligibility, the pair is persistently flagged restricted-for-this-
  account: its edge cards and wallet entries show a "restricted in your
  region" badge, the execute button disappears for it, and its future
  paper bookings are labeled pretend-only so the wallet's score stops
  implying money the user could never have taken. The flag is per
  account/venue, clearable server-side if the account's status changes.
- UI: the wallet's "I traded this" button becomes "Execute for real" on
  live-enabled servers, opening a confirm dialog that restates both
  legs, the re-priced cost, the cap, and the risk line. Every execution
  is one explicit human click — no auto-trading from the web app in v1.
- Real fills mark the wallet entry acted with *actual* fill dollars
  (replacing the honor-system numbers) and append to `trade_log`.

### Phase 4 — Reconciliation & risk

- Positions view from venue APIs (Kalshi portfolio, Polymarket
  positions) vs. what `trade_log` expects; differences are loud.
- Legging policy: if leg 2 fails, attempt immediate cancel of leg 1's
  remainder; if partially filled, surface a one-sided-position alert in
  the UI (auto-unwind is out of scope for v1 — a human decision).
- Only after this phase: consider letting the CLI bot's timer loop run
  live unattended.

## Explicitly out of scope (v1)

- Auto-trading without per-order confirmation from the web UI.
- Market orders (limit-at-detected-ask only, as today).
- Polymarket US eligibility engineering — see open questions.
- Auto-unwind of one-sided positions.

## Open questions for the reviewer

1. **Polymarket eligibility/geo — RESOLVED 2026-08-04**: the manual
   test confirmed US persons cannot trade on Polymarket at all. See
   "Eligibility finding and execution modes" above: assisted mode for
   US accounts (user places the Polymarket leg or skips it; app
   automates only the Kalshi hedge, after confirmation), auto mode
   deferred until an eligible non-US account exists, and no
   geoblock-circumvention features in any mode. Nothing here is legal
   or financial advice.
2. **Sidecar vs native EIP-712** (Phase 2 A/B): is a Python sidecar
   acceptable operationally, or is single-binary a hard requirement?
3. **Fee truth**: `fees.ml` returns literally `Price.zero` for
   Polymarket taker fees. Zero happens to match Polymarket's current
   CLOB schedule, but nothing verifies it, and `min_edge` (and every
   wallet booking) silently depends on it. Verify against the live
   schedule before real money relies on it, and decide where a fee
   change would be caught.
4. **Custody**: is the Polygon private key generated fresh for this app
   (recommended — small float, blast radius limited) or the user's main
   wallet (not recommended)?
5. **Partner constraints**: `lib/types`/`lib/protocol` must stay
   JS-linkable and partner-owned surfaces additive-only (standing
   project rule) — the plan respects this, but two of its changes touch
   surfaces Arthur's work also takes and should be sequenced with him:
   the `Executor.live` signature change (his bots take an
   `Executor.t`), and Phase 3's additive `Protocol` wire types plus
   sqlite schema additions (restricted-pair flags, execute RPCs,
   trade_log).

## Suggested milestone order

0. Rails (Phase 0) — no live code until the log, caps, and kill switch
   exist.
0.5. Fix the legging abort + its comment (`Bot.execute`) — **done
   2026-08-04**, with a regression expect-test (`test_bot.ml`): a paper
   executor whose first leg is unaffordable proves the hedge is never
   sent (cash untouched, NO position zero, `legs_not_sent 1`).
1. Kalshi demo round-trip (Phase 1).
2. Kalshi real, 1-contract canary, CLI only.
3. Polymarket sidecar demo round-trip (Phase 2A).
4. Web confirm-and-execute surface (Phase 3), Kalshi first.
5. Both-legs live execution of one real, human-confirmed arb.
6. Reconciliation (Phase 4).
