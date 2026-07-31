(** Text front end for the bot-configuration language: one statement per
    line, parsed in order into {!Rule.t}s ready for {!Program.create}.

    Statements (see [bot_creation/bot_creation_spec.md]):

    {v
    NAME = <numeric or boolean expression>
    [EVERY n(m|h|d)] [WHEN I (BUY|SELL) slug] IF <bool> THEN <action> [ELSE <action>]
    [EVERY n(m|h|d)] [WHEN I (BUY|SELL) slug] <action>   (bare action needs a qualifier)
    v}

    where an action is [(BUY|SELL) <numeric expr> slug (YES|NO)] and a signal
    statement is
    [slug (YES|NO) (UP|DOWN) BY <number or pct%> SINCE n(m|h|d) [AGO] [END n(m|h|d) [AGO]]].

    Conventions the grammar commits to:
    - Keywords ([IF], [BUY], [SINCE], ...) are uppercase; [true]/[false] are
      lowercase. Anything else alphabetic is a slug or variable name.
    - Variables are referenced with a [$] prefix ([$cash]) and defined bare
      ([lot = 5]); definitions must precede uses (single pass).
    - [BY 3%] is a percent move, [BY 0.03] an absolute dollar move.
    - Boolean operator precedence, tightest first: [!], [&&], [^], [||].
      Numeric: [*] [/] over [+] [-]; unary [-]. Parentheses group both.
    - Names may contain hyphens ([save-act_price]), so subtraction needs
      spaces around a [-] that follows a name: [$a - $b], not [$a-b].
    - Comparisons ([> < >= <= == !=]) accept full numeric expressions on both
      sides; [=] alone is the definition sign, never a comparison.

    Whether an expression is numeric or boolean is inferred: a definition is
    first parsed as a whole-line numeric expression and falls back to
    boolean, and [$name] resolves through {!Var_env} kind checks.

    [slugs] seeds the built-in variables ([cash], [<slug>_price], ...);
    slug-membership and span validation stay {!Program.create}'s job. *)

open! Core
open Types

val program : string -> slugs:Slug.t list -> Rule.t list Or_error.t
