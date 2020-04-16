# CSCI-400 - Lab #5

In this lab, we will continue building our interpreter for a simple
subset of JavaScript. We will use the same grammar as Lab 4:

- **program** *p* ::= *e* | `const` *x* `=` *e* `;` *p*

- **block** *bl* ::= `return` *e* | `const` *x* `=` *e* `;` *bl*

- **expression** *e* ::= *x* | *v* | *uop* *e* | *e* *bop* *e*
                       | *e* `?` *e* `:` *e* | `console.log` `(` *e* `)` | *e* `(` *e* `)`

- **value** *v* ::= *n* | *b* | *s* | `undefined` | `function` *x* `(` *x* `)` `{` *bl* `}`

- **unary operator** *uop* ::= `-` | `!`

- **binary operator** *bop* ::= `+` | `-` | `*` | `/` | `===` | `!==` | `<` | `<=` | `>` | `>=` | `&&` | `||`

- **identifier** *x*

- **number (float)** *n*

- **boolean** *b* ::= `true` | `false`

- **string** *s*

## Task 1

Your first task is the following:

Edit the file `Evaluator5.scala` to complete the `typecheck` function.
There are a few cases where JavaScript cannot perform auto-conversion
of types, and these should result in a `StaticTypeError`.

For simplicity, you can require that conditionals `c ? e1 : e2` and
boolean binary operators `e1 && e2` and `e1 || e2` have the have the
same type for `e1` and `e2`, (e.g., you can throw a `StaticTypeError`
for expressions such as `c ? "hello" : 123` and `true && "hello"`).

Add at least 5 new *non-trivial* tests for your type checker
functionality.

## Task 2

Your second task is the following:

Compared to the previous lab, we now want to support *static scoping*,
rather than the dynamic scoping we implemented previously. Complete
the `eval` function as in Lab 4, but now add support for static
scoping.  The `ClosureExpr` data type allows the construction of a
*closure* for each lambda expression.  Finally, add at least 5 new
*non-trivial* tests in `Evaluator5Test.scala` for your evaluator.

Note: you can use the Node.js JavaScript interpreter to check the
results of your interpreter (this is the nodejs command on Linux).

## Task 3

Your final task is the following:

Recall the static/dynamic scoping example from class. With static
scoping, the following JavaScript expression should evaluate to `11`
(5 + 6). Note that the curly braces are only used to prevent
JavaScript from complaining about re-declaring variable `x`.

```
const x = 5;
const f = (y) => x + y;
{
const x = 7;
f(6)
}
```

Implement the above example using your `Expr` abstract datatype, and add it as
a 4th test case for your `eval` function (your Lab 4 code should produce
`13` for this expression (7 + 6), while your Lab 5 code should produce
`11` (5 + 6), since we have now implemented static scoping).
