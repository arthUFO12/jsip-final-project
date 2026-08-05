# Merging `user_bot_creation` + `main`, and putting arbitrage in the UI

Notes for the merge conversation. `main` has the arbitrage/execution stack;
`user_bot_creation` has the server/client/bot-language stack. Merge base is
`443af3d`. Nothing here is done yet — this is the proposed strategy.

## Conflicts to agree on

| Where | What happened | Proposed resolution |
|---|---|---|
| `lib/types/src/market_stub.{ml,mli}` | Both sides added `category` (different positions); branch also adds `volume : Volume.t option`; main adds `created_time`, non-optional `close_time` | One unified record with all fields; pick one canonical field order and fix all constructors once |
| `lib/database/src/database_types.ml` | Both restructured the Caqti row around the `t8` ceiling, incompatibly | Rebuild once for the unified stub (category + volume + created_time + close_time); column order follows the record |
| `lib/types/src/{side,contract_type,category,volume}` | Different derive lists | Union of derives — the protocol needs `bin_io`, arbitrage needs `sexp_of` |
| `lib/types/src/action.{ml,mli}` | Branch removed the `name` field and `perform_action` | Accept the removal; main-side constructors drop `~name` (main's arbitrage bot doesn't use `Action.t`, so impact is small) |
| `lib/types/src/dune` | **The big one.** Branch strips `types` to core-only so the Bonsai client (OCaml, compiled to a browser bundle by js_of_ocaml) can link it; main's dune lists `async cohttp-async yojson`, which js_of_ocaml can't compile | Keep `types` core-only. Any main-side type module that pulls Async/cohttp moves out of `types` or drops the dep. Needs an audit of `pairs/policy/sim/binary_book/pair_proposal` — most look pure |
| `dune-project` / `arbiter.opam` | Both edited dep lists | Union (both are on caqti already, so this is textual) |
| `arbiter.db`, `test.db` | Binary DBs checked in on the branch | Delete from git, add to `.gitignore` (main already dropped `test.db`) |

Recommended mechanics: do the merge on a branch and land it as a PR so both
of you sign off on the schema and field-order choices.

## Design fork the merge exposes (decide, don't solve, at merge time)

Main's arbitrage detector prices **live order books with depth and fees**
(`Detect.Leg`, `Binary_book`, `Executor`). The branch's simulator prices
**interpolated mid-price time series with no depth**. Both are legitimate;
they serve different purposes (trading vs. backtesting). The merge doesn't
have to reconcile them — but the UI work below deliberately keeps them
separate rather than pretending backtest fills are executable edges.

## Arbitrage in the UI — additive only, nothing replaced

### 1. Pairs review page (new page, new RPCs)

- `Protocol` gains `list-pair-proposals : unit -> Pair_card.t list Or_error.t`
  and `decide-pair : Decide_request.t -> Pair_card.t Or_error.t`, where
  `Pair_card.t` carries index, both titles + venues, score, LLM explanation,
  status (wire-friendly: strings/floats, `bin_io`).
- Server backs both with `Arbitrage.Review` (already factored out of the CLI
  so the CLI and the RPC share one implementation and one numbering).
- Client gains `Page.Pairs` alongside `Markets | Bots` — a list with
  approve/reject buttons. Existing pages untouched; the CLI `review` verb
  keeps working.

### 2. Arbitrage as a backtestable strategy (inside the existing wizard)

The bot language can already express a cross-venue spread — tickers are
numeric expressions and NO = 1 − YES, so
`IF kalshi-x + (1 - poly-y) < 0.98 THEN BUY 10 kalshi-x YES` is valid today.
What's missing is plumbing, all additive:

- `seed_database` seeds Polymarket stubs too (today: Kalshi only).
- `resolve_stubs` accepts Polymarket markets (today it requires
  `series_ticker`, a Kalshi-only concept; Polymarket routes via
  `clob_token_id`).
- `Market_card.t` gains a `venue` field so the picker can show it
  (`Venue` needs `bin_io`).
- Optional sugar: an "arbitrage template" button on an approved pair that
  opens the wizard pre-filled with the two markets and a spread program —
  connects the review page to the backtester using the wizard exactly as
  built.

### Sequencing

1. Agree on conflict strategy above → merge PR.
2. Pairs review page (RPCs + page) — smallest, self-contained.
3. Polymarket in the seed/resolver + `venue` on `Market_card`.
4. Arbitrage template wiring review → wizard.
