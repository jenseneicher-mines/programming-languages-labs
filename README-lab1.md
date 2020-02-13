# CSCI-400 Lab #1

Your goal in this lab will be to add Scala code to complete the
functionality described in `ListDemo.scala` and `TreeDemo.scala`.
Skeleton code is provided, and you will need to fill in the body of
several functions. Each location where you need to replace a
placeholder expression with your own code is marked with a `TODO`
comment.

## Task 1

In this part of the lab, we will revisit the list-processing functions
discussed in class. The functional programming paradigm makes heavy
use of these (higher-order) functions, such as `map` (for transforming
a list into a new list), `fold` (for performing a computation over the
list), etc.

In later labs, we will use Scala's built-in versions of these
functions, but for now, we will consider how to *implement* these
functions on our owns using only *recursive higher-order functions*
and *pattern matching*.

- Edit `ListDemo.scala` at the locations indicated by `TODO` comments,
  and complete the functions as described in the comments.
- Add at least 5 new *non-trivial* unit tests per function in
  `ListDemoTest.scala`.

DO NOT USE SCALA's BUILTIN `map`, `fold`, OR `filter` IN THIS PROJECT.

## Task 2

In this part of the lab, we will revisit our discussion of abstract
data types and using these to encode binary trees. We will write some
functions that apply to *arbitrary* binary trees, and we will then
turn our attention to *binary search trees*, a special form of binary
trees.

Binary search trees are binary trees which satisfy a key property:
**for any node, all elements in its left subtree are less than that
node, and all elements of its right subtree are greater than that
node** (note that in this case, we do not allow duplicate values to be
stored in the binary search tree). As its name implies, binary search
trees are good for fast lookups (we can get log(n) performance if the
tree is properly balanced).

In this part of the lab, you will write functions to manipulate binary
search trees.

- Edit `TreeDemo.scala` at the locations indicated by `TODO` comments,
  and complete the functions as described in the comments.
- Add at least 5 new *non-trivial* unit tests per function in
  `TreeDemoTest.scala`.
