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


;; question: can this rule work without the strip-fallback?
(fallback fbg g) -> fbg
   if   strip-fallback(g) -> s1   and    strip-fallback(g) -> s2   and s1 =/= s2

;; in this rule, g -> s1 may have included a fallback reduction
(fallback fbg g) -> s1
   if g -> s1   and not(exists s2 st. g -> s2 and s1 =/= s2)

This is nondeterministic, and always allows that we just do the outermost fallback. So not
useful for understanding how specialized your program is, but it is enough for proving the theorem.
