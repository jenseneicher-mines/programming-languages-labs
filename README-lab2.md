# CSCI-400 Lab #2

Your goal in this lab will be to add Scala code to complete the
functionality described in `Evaluator2.scala`.  Skeleton code is
provided, and you will need to fill in the body of several
functions. Each location where you need to replace a placeholder
expression with your own code is marked with a `TODO` comment.

## Task

In this part of the lab, we will begin building our JavaScript
interpreter.  We will start by building a function which evaluates
JavaScript *expressions*.  For example, when given a JavaScript
expression such as `1+1`, your code should return the value `2`.

For now, we will only consider *well-typed* expressions, such as the
above.  In this lab, expressions such as `true + 1` will be considered
an error, both in our expression evaluator, and in our type checker.

Here are the expressions that should be supported:

- **expression** *e* ::= *v* | *uop* *e* | *e* *bop* *e*

- **value** *v* ::= *n* | *b*

- **unary operator** *uop* ::= `-` | `!`

- **binary operator** *bop* ::= `+` | `-` | `*` | `/` | `===` | `!==` | `<` | `<=` | `>` | `>=` | `&&` | `||`

- **number (float)** *n*

- **boolean** *b* ::= `true` | `false`

Beyond evaluating expressions, you should also implement a basic
bottom-up typechecker for expressions. For example, when given a
JavaScript expression such as `1 + (2 + 3)`, your typechecker should
indicate that this expression is of type *num*.

The types needed for the above expressions are `num` and `bool`.

- Edit `Evaluator2.scala` at the locations indicated by `TODO`
  comments, and complete the functions described in the comments.
- Add at least 5 new *non-trivial* unit tests per function in
  `Evaluator2Test.scala`.
