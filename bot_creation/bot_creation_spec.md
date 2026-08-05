# Bot creation spec

Bot creation will be available to users through a command line or web interface where they enter coding language-like text. Here is the spec for that language.

Keywords are canonically lowercase (`if`, `buy`, `since`, ...) and matched case-insensitively.

Tickers are written in normalized form: the venue slug lowercased with every `-` replaced by `_`. For example Kalshi's `KXELONMARS-99` is typed `kxelonmars_99`, and a double hyphen becomes a double underscore (`KXALBUM--26OCT01` → `kxalbum__26oct01`). Raw single-hyphen spellings like `save-act` also resolve.

There are 2 types of statements. A rule defining statement and a variable defining statement. 
There are two types of variables. Numeric variables and boolean variables. 
A boolean statement has a couple of types:
((NUMERIC VARIABLE|CONSTANT) (>|<|>=|<=|==|!=) (NUMERIC VARIABLE|CONSTANT))
SLUG (up | down) by ABSOLUTE_NUMBER since (CONSTANT NUMBER)(m|h|d) ago <end (CONSTANT NUMBER)(m|h|d) ago>
SLUG (up | down) by PERCENTAGE since (CONSTANT NUMBER)(m|h|d) <end (CONSTANT NUMBER)(m|h|d) ago>

These signal statements always track the market's yes price. A move in the no price is written as the opposite move: what used to be `no down by 0.10` is `up by 0.10`. (For percentage moves the base is the yes price at the window start.)

Variable defining statments look like this
```
VARIABLE_NAME = (NUMERIC EXPR | BOOLEAN EXPR)
```
A numeric expression is an expression containing constant numbers, numeric variables joined by *, +, /, or -, or other numeric expressions. A boolean expression is an expression containing true, false, boolean statements, boolean variables joined by &&, ||, ^, or !, or other boolean expressions. Any expression not in these forms should cause an error.
To use variables in expressions, a $ must be before its name.

Rule defining statements have a couple of forms. For reference, an action statment is in the form:
```
(buy | sell) (SIZE) (SLUG) (yes|no)
```
Note size can be a variable expression.

The main form of is:
```
if (BOOLEAN EXPR) then ACTION_STATMENT <else ACTION_STATEMENT>
```

Optionally you can add a qualifier before the statement. There's two types of qualifiers. An every qualifier is:
```
every (CONSTANT NUMBER)(m|h|d)
```

A when I qualifier is:
```
when i (buy | sell) (SLUG)
```

Both of these clauses can appear before the main form. An every clause means the if statement is only invoked on every interval stated in the every
statement. A when i clause will cause the if statement to only be invoked when a certain ticker is bought or sold.
Optionally, it is allowed to put an action statement directly after one of these statements:

```
every (CONSTANT NUMBER)(m|h|d) ACTION_STATEMENT
```

There should be a couple numeric variables already available to the user: cash, realized, and unrealized (referenced `$cash` etc.).

Markets are referenced by ticker directly in numeric expressions:

- A bare ticker (e.g. `save-act`), or equivalently `price of save-act`, is the market's current yes price. The no price is written arithmetically: `1 - save-act`.
- `inventory of save-act` is the signed number of contracts currently held in that market: positive long yes, negative long no, 0 when flat.
- `avgcost of save-act` is the average price paid per contract of the open position in that market, 0 when flat.

Ticker names are reserved — a variable definition may not reuse one.