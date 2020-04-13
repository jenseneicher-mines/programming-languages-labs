# CSCI-400 - Lab #4

In this lab, we will continue building our interpreter for a simple
subset of JavaScript. The following is the updated grammar for this
language.  Notice that we will now add support for *recursive
higher-order functions*.

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

# Task

Edit the file `Evaluator4.scala` to complete the `eval` function.
Compared to the previous lab, we now need to support:
1.  strings,
2. (recursive) function definitions and function calls, using *dynamic
   scope*.

Finally, add at least 5 new *non-trivial* tests in `Evaluator4Test.scala` for your evaluator.

Note: you can use the Node.js JavaScript interpreter to check the results of
your interpreter (this is the nodejs command on Linux).
