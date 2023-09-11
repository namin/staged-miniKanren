# Semantics

## Staging Correctness Theorem

```
erase(sg): fix(g)
erase(g(sg)) = g(erase(sg))
erase(later(lg)) lg
erase(fallback sg) = erase(sg)
erase(specialize-partial-apply t r t ...) = (partial-apply t r t ...)
```

`[(staged sg)] ~=~ [(erase sg)]` if staging terminates.
The denotation `[]` is an answer set.
For terminating queries, thetheorem is:
`(run* (staged sg)) ~=~ (run* (erase sg))`.

Another way:

```
staged(sg) = rg
[rg] = [erase(sg)]
```

`staged(sg)` produces a constraint store state and a program, and reifies it into an `rg`.

How do we define the denotation `[]` and how do we define `staged`?
