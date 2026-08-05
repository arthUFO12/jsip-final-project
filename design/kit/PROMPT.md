# Prompt for claude.ai/design — Arbiter design system

Paste everything below the line into the Claude Design project that holds
this kit. It briefs the designer-Claude on what the product is, what it may
change, and what it must never touch.

---

## What this product is

**Arbiter** is a prediction-market app with two personalities living in one
dark UI:

1. **A paper-money playground.** Browse live Kalshi markets by category,
   build a trading bot in a three-step wizard (pick markets → write rules in
   a tiny DSL with autocomplete → run a backtest), and study the results:
   stat tiles, P&L/value/price/inventory line charts, a trade log, and an
   optional "dumb bot average" baseline to beat. There's also a pretend
   wallet that books every arbitrage edge the scanner finds as if you'd
   taken it — a scoreboard for "how much would I have made?", like trading
   on a Kalshi demo account.
2. **A real-money path.** An arbitrage pipeline (sweep both venues → review
   candidate pairs, by hand or LLM → price approved pairs on live order
   books) that ends in an assisted-hedge dialog which can place a real
   Kalshi order, and a Wallet page that manages an encrypted trading key.

Four pages: **Markets, Bots, Arbitrage, Wallet**. Every screen and state is
reproduced in this project's cards with real DOM and real copy.

## The direction

**Dark fintech, but inviting.** Keep the identity: deep navy surfaces with
a sky→violet gradient accent. Aim for the confidence of a good trading
terminal crossed with the warmth of a game you want to poke at —
*not* a sterile Bloomberg clone, and *not* a toy. The current look is the
starting point in `tokens.css`/`kit.css`; it works but reads flat and
generic. Make it feel designed: real hierarchy, deliberate spacing, richer
surfaces (subtle elevation/glow are welcome), micro-transitions on hover
and state changes.

**Where playfulness is welcome:** empty states, loading/waiting copy
moments (card 18), the pretend-wallet scoreboard, the results reveal after
a backtest, gradients and hover motion.

**Where playfulness is banned:** numbers and money values, error and
warning text, the assisted-hedge dialog (card 19), and the wallet key
disclaimers. Those flows move real dollars — keep them sober, high
contrast, unambiguous.

## Hard rules (constraints, not suggestions)

1. **Class names and DOM structure are frozen.** The app is generated from
   OCaml that emits exactly these class names. Restyle anything; never
   rename, remove, or require new classes/wrappers. All edits land in
   `tokens.css` and `kit.css` only. Card HTML may only change where a card
   itself says it is demo chrome (`ds-*` classes).
2. **Every design decision is a CSS variable in `tokens.css`.** No raw hex,
   px radius, or font stack in `kit.css` — if you need a new value, add a
   token first.
3. **Dark only.** No light theme, no `prefers-color-scheme` work.
4. **Self-contained CSS.** No frameworks, no preprocessors, no imports, no
   webfonts (system stack only — if you feel strongly about a font, note it
   as a suggestion instead of adding one).
5. **Charts stay token-styled inline SVG** (card 15 shows the anatomy).
   Series identity comes from `.series-1…5` setting `color`; strokes and
   legend keys inherit `currentColor`.

## Tokens you must deliver (complete inventory)

- Surface stack: page base + glow, `--panel`, `--panel-deep`,
  `--panel-raised`, `--chip-bg`, plus border trio (`--border`,
  `--border-soft`, `--divider`).
- Ink scale: `--ink`, `--ink-body`, `--ink-soft`, `--ink-muted`,
  `--ink-faint`, `--ink-ghost`, `--ink-list`.
- Brand: `--brand-a/b` (text gradient), `--brand-strong-a/b` (filled
  gradient), `--accent`, `--accent-alt`, focus-ring treatment.
- Status: `--pos/--neg/--warn` with their `-bg`/`-border`/`-strong`
  variants. Status colors must stay visually distinct from chart series.
- **Chart series `--series-1…5`: must pass the bundled validator.** Card 15
  runs it in the browser console automatically; the current values pass.
  The old app palette failed hard (its blue/violet pair measured ΔE 1.3
  under deuteranopia — indistinguishable). If you change any series hex,
  re-check the console on card 15 and only ship a passing set
  (`--mode dark --surface #10151f`, all five checks green).
- The 12 category accents `--cat-*` (may be re-stepped, must stay 12
  distinct hues).
- Radius scale, space scale, type scale, motion durations.

## States to design (not optional)

- Hover, active, disabled, and `:focus-visible` for every control (cards
  05–06).
- Invalid input: `.input-invalid` + `.field-error` (card 06).
- Request lifecycle: quiet running line (`.status`), result summary
  (`.arb-summary`), mono error block (`.error-box`), dismissible
  `.error-banner` with retry (card 17).
- Chart hover: crosshair + single all-series tooltip, value-first rows
  (card 15); empty and single-series chart states (card 16).

## Responsive spec

One breakpoint at **720px** is already wired: rules-layout collapses to a
single column, nav wraps, side paddings shrink. Keep every card usable at
**375px and 1280px** — that's the acceptance test. The market-card grid is
`auto-fill minmax(220px, 1fr)`; charts are 100%-width SVGs; the fills table
scrolls inside `.fills-table-wrap` rather than stretching the page.

## Chart language

Recessive gridlines (they must never compete with data), 4–5 y-ticks on a
1-2-5 "nice" progression, time-labeled x-ticks, shaded warmup band on the
left, 2px solid lines for the user's bot, same hues dashed for the dumb-bot
baseline, legend with short line-style keys (never filled boxes), no legend
when a chart has a single series.

## Acceptance checklist (verify before you finish)

- [ ] Every card renders correctly at 375px and 1280px.
- [ ] Card 15's console shows the palette validator passing (no FAIL rows).
- [ ] `grep`-level check: no class selector was renamed or removed in
      `kit.css`; no hex/px literals crept into `kit.css` that aren't
      `var(...)` (hairline exceptions like `1px` borders are fine).
- [ ] The hedge dialog and wallet disclaimers read sober and high-contrast.
- [ ] Empty/loading states got the warmth pass.
