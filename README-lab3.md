# CSCI-400 - Lab #3

In this lab, we will use the skills we developed in Lab 2 to continue
building an interpreter for a simple subset of JavaScript. The
following is the grammar for this language. Notice that we will now
add support for *immutable variables*, *conditionals*, and a *console
print operator*:

- **program** *p* ::= *e* | `const` *x* `=` *e* `;` *p*

- **expression** *e* ::= *x* | *v* | *uop* *e* | *e* *bop* *e*
                | *e* `?` *e* `:` *e* | `console.log` `(` *e* `)`

- **value** *v* ::= *n* | *b* | `undefined`

- **unary operator** *uop* ::= `-` | `!`

- **binary operator** *bop* ::= `+` | `-` | `*` | `/` | `===` | `!==` | `<` | `<=` | `>` | `>=` | `&&` | `||`

- **identifier** *x*

- **number (float)** *n*

- **boolean** *b* ::= `true` | `false`

Your tasks are twofold:

## Task 1

Edit the file `SimpleParser.scala`. Your goal is to use Scala's Parser
Combinators to build a parser for a subset of the JavaScript binary
expressions.  Your parser will return an Expr which can be used in
your evaluator.

Add at least 5 new *non-trivial* tests in `SimpleParserTest.scala` for
your parser.

## Task 2

Edit the file `Evaluator3.scala` to complete the `eval` function.  In
the previous lab, we could only evaluate simple arithmetic expressions
-- in this lab, we will add support for variables, conditionals, and a
"print" operator.

Add at least 5 new *non-trivial* tests in `Evaluator3Test.scala` for
your evaluator.

Note: you can use the Node.js JavaScript interpreter to check the results of
your interpreter (this is the `nodejs` command on Linux).
