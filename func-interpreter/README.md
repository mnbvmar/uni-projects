Functional language interpreter
===


This implements a simple functional language interpreter in Haskell.
The language grammar can be found in `lang.cf`
  (the structure of the file describes the language pretty clearly).

The interpreted program defines an entry point `main` that
  returns a value of any type, which is then presented to the user.

Examples of the correctly parsed and interpreted files:

```
a b c = b + c; # Define a function adding two integers.
main = a 5 7; # Return 5 + 7 = 12.
```

```
# Recursive definition of a binary rooted tree.
data Tree [] = Leaf[] | Node[Tree, Tree, Int];

# Get the sum of the values in the tree.
def getSum :: Tree -> Int;
getSum tree = case tree {
    Leaf [] -> 0
  | Node [lft rgt val] -> (val + (getSum lft) + (getSum rgt))
};

# Should output 10.
main = getSum (Node (Node Leaf Leaf 1) (Node Leaf (Node Leaf Leaf 4) 2) 3);
``



Running
---

Dependencies: `ghc bnfc happy alex`.

To compile the interpreter, just run `make`.
It should compile with a reasonably new Haskell compiler version.

Programs can be fed through the standard input or loaded from the file
when provided as a command line argument.


Features included
---

* Constructor types (possibly recursive ones);
* (Recursive) pattern matching on constructor types; performs exhaustiveness
    pattern matching checks (as in `bad/Lang34.in`).
* Type declarations can be provided for the top-level definitions.
* Static type checking is performed at a pre-interpreting phase.
* Some list processing functionality is added, both in interpreter
    and as a part of the standard library:
** Lists can be constructed using either constructors or a comma-separated list.
** Apart from basic functionality, some higher-order functions are added:
      `map`, `filter`, and folds.
* Standard library also contains the basic functionality of `Maybe` and `Either`
    constructs known from Haskell; functions `doMaybe` and `doEither` can
    apply monadic operations to them.
