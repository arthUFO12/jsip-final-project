# Bot creation spec

Bot creation will be available to users through a command line or web interface where they enter coding language-like text. Here is the spec for that language.

There are 2 types of statements. A rule defining statement and a variable defining statement. 
There are two types of variables. Numeric variables and boolean variables. 
A boolean statement has a couple of types:
((NUMERIC VARIABLE|CONSTANT) (>|<|>=|<=|==|!=) (NUMERIC VARIABLE|CONSTANT))
SLUG (YES | NO) (UP | DOWN) BY ABSOLUTE_NUMBER SINCE (CONSTANT NUMBER)(m|h|d) AGO <END (CONSTANT NUMBER)(m|h|d) AGO>
SLUG (YES | NO) (UP | DOWN) BY PERCENTAGE SINCE (CONSTANT NUMBER)(m|h|d) <END (CONSTANT NUMBER)(m|h|d) AGO>

Variable defining statments look like this
```
VARIABLE_NAME = (NUMERIC EXPR | BOOLEAN EXPR)
```
A numeric expression is an expression containing constant numbers, numeric variables joined by *, +, /, or -, or other numeric expressions. A boolean expression is an expression containing true, false, boolean statements, boolean variables joined by &&, ||, ^, or !, or other boolean expressions. Any expression not in these forms should cause an error.
To use variables in expressions, a $ must be before its name.

Rule defining statements have a couple of forms. For reference, an action statment is in the form:
```
(BUY | SELL) (SIZE) (SLUG) (YES|NO)
```
Note size can be a variable expression.

The main form of is:
```
IF (BOOLEAN EXPR) THEN ACTION_STATMENT <ELSE ACTION_STATEMENT>
```

Optionally you can add a qualifier before the statement. There's two types of qualifiers. An every qualifier is:
```
EVERY (CONSTANT NUMBER)(m|h|d)
```

A when I qualifier is:
```
WHEN I (BUY | SELL) (SLUG)
```

Both of these clauses can appear before the main form. An every clause means the if statement is only invoked on every interval stated in the every
statement. A when i clause will cause the if statement to only be invoked when a certain ticker is bought or sold.
Optionally, it is allowed to put an action statement directly after one of these statements:

```
EVERY (CONSTANT NUMBER)(m|h|d) ACTION_STATEMENT
```

There should be a couple numeric variables already available to the user: cash, realized, and unrealized (referenced `$cash` etc.).

Markets are referenced by ticker directly in numeric expressions:

- A bare ticker (e.g. `save-act`), or equivalently `PRICE save-act`, is the market's current YES price. The NO price is written arithmetically: `1 - save-act`.
- `INVENTORY save-act` is the signed number of contracts currently held in that market: positive long YES, negative long NO, 0 when flat.

Ticker names are reserved — a variable definition may not reuse one.