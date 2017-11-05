# staged-miniKanren

This project explores multi-stage logic programming in miniKanren.
While partial evaluation has a rich history in both functional and logic programming,
multi-stage programming has so far only been explored in a functional/imperative setting,
with many success stories in high-performance computing.
How can we add staging constructs to a logic / relational programming language like miniKanren?
And what can we do with such constructs?
Can we turn an interpreter for a functional language written in miniKanren into a compiler translating any functional program into a relational one?
Can we go beyond and collapse towers of interpreters, covering hybrid paradigms?
What happens when a staged program is run "backwards"?
These are the sorts of questions we want to answer.

## Prototype

Our starting point is [the canonical miniKanren](https://github.com/miniKanren/miniKanren), which supports logical operators `==`, `fresh`, `conde`, interface operators `run`, `run*` and constraint operators `=/=`, `symbolo`, `numbero`, `absento`.

We extend miniKanren with staging operators `lift` and `lift-scope`. `lift` records some term for later, `lift-scope` takes a goal and a variable and runs the goal accumulating all lifted terms into the variable and discarding the local context.

### Tests

Our starting point is [a full interpreter in miniKanren for a subset of Racket](https://github.com/webyrd/faster-miniKanren/blob/master/full-interp.scm).
We [stage this interpreter](staged-interp.scm), and observe via [example runs](tests.scm) whether we can reasonably turn functional programs into relational programs without interpretation overhead.

We use the `lift` operator to defer unifications so that the generated program can perform all necessary constraints. We use `lift-scope` so that we can generate code for `if`/`conde` branches. We want to keep the generated relational code first-order, and so a good strategy is to not lift functions and applications. For recursively defined functions, we use explicit folding. For convenience, we change the interface of the interpreter so that the eval and lookup functions, `eval-expo` and `lookupo`, take an extra first parameter `stage?` which allows us to control whether we want/expect lifted or unlifted values.

To be continued...
