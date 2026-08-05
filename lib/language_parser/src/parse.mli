(** Text front end for the bot-configuration language: one statement per
    line, parsed in order into {!Rule.t}s ready for {!Program.create}.

    Statements (see [bot_creation/bot_creation_spec.md]):

    {v
    name = <numeric or boolean expression>
    [every n(m|h|d)] [when i (buy|sell) ticker] if <bool> then <action> [else <action>]
    [every n(m|h|d)] [when i (buy|sell) ticker] <action>   (bare action needs a qualifier)
    v}

    where an action is [(buy|sell) <numeric expr> ticker (yes|no)] and a
    signal statement is
    [ticker (up|down) by <number or pct%> since n(m|h|d) [ago] [end n(m|h|d) [ago]]].
    Signals always track the market's yes price — a fall in the no price is
    written as the equivalent [up] move.

    Conventions the grammar commits to:
    - Keywords ([if], [buy], [since], ...) are canonically lowercase and
      matched case-insensitively (see {!Ticker_name.keywords}). Anything else
      alphabetic is a ticker or variable name.
    - Tickers are written in {!Ticker_name.normalize}d form — lowercase with
      every [-] replaced by [_] ([KXELONMARS-99] is typed [kxelonmars_99]);
      raw single-hyphen spellings also resolve. A ticker that would collide
      with a keyword or another ticker after normalization makes the whole
      program an error.
    - Variables are referenced with a [$] prefix ([$cash]) and defined bare
      ([lot = 5]); definitions must precede uses (single pass).
    - [by 3%] is a percent move, [by 0.03] an absolute dollar move.
    - Boolean operator precedence, tightest first: [!], [&&], [^], [||].
      Numeric: [*] [/] over [+] [-]; unary [-]. Parentheses group both.
    - Names may contain hyphens ([save-act]), so subtraction needs spaces
      around a [-] that follows a name: [$a - $b], not [$a-b].
    - Comparisons ([> < >= <= == !=]) accept full numeric expressions on both
      sides; [=] alone is the definition sign, never a comparison.

    Whether an expression is numeric or boolean is inferred: a definition is
    first parsed as a whole-line numeric expression and falls back to
    boolean, and [$name] resolves through {!Var_env} kind checks.

    Market references are numeric expressions: a bare ticker ([save_act]) or
    [price of save_act] is the market's current Yes price,
    [inventory of save_act] is the signed contracts held (positive long Yes,
    negative long No), and [avgcost of save_act] is the average price paid
    per contract of the open position ([0] when flat). The No price is
    written arithmetically: [1 - save_act]. Ticker names are reserved —
    defining a variable with a ticker's name is an error.

    [slugs] decides which words are tickers and seeds nothing else; the
    parser resolves normalized names back to these original slugs, so
    everything downstream sees the venue's real slug. The built-in variables
    are [$cash], [$realized], and [$unrealized]. Span validation stays
    {!Program.create}'s job. *)

open! Core
open Types

val program : string -> slugs:Slug.t list -> Rule.t list Or_error.t
