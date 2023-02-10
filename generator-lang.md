staged-miniKanren's Generator Language
--------------------------------------

## `run-staged`

The special form `run-staged` runs a staged relational program in two steps: the first steps produces generated code, and the second step runs the generated code.

## `later` miniKanren

For code to be part of the generated code, it needs to be marked as `later`. This is done by prepending an `l` in front of miniKanren primitives, `l==` for example.

Some `later` forms requires explanation:

- `(lapp unstaged-relation args ...)` represents a relation call that will occur in the later stage. The invoked relation should be plained miniKanren code, not using any staged-miniKanren features.

- `(lconde ((generator-goal ...) ...))` will produce a `conde` in the generated code with the given number of branches. This does not cause any branching at compile time, rather all the generator goals in branch are executed each in a separate context to generate the later-stage branch.

## `condg`

The special form `condg` aims to simplify writing generators which must be deterministic in the first stage: all the non-determinism is accumulated in the later code.

The grammar of `condg` is:
```
(condg
generator-goal
([variable ...] [generator-goal ...] [generator-goal ...]) ...)
```

Let's name this as follows:
```
(condg
fallback
(vars guard branch) ...)
```

Semantically:
- All guards fail: this is a staging-time error.
- Only one guad succeeds and produces only one answer: its branch is taken.
- Otherwise, the fallback is taken.

## reified partial applications




