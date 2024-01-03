# Semantics

The grammar is defined at the end of [lang.md](lang.md#grammar).

## Staging Correctness Theorem

```
erase(sg): fix(g) // g(g(g(...)))
erase(g(sg)) = g(erase(sg))
erase(later(lg)) = lg
erase(fallback sg) = erase(sg)
erase(specialize-partial-apply t r t ...) = (partial-apply t r t ...)
```

`[(top_staged sg)] ~=~ [(erase sg)]` if staging terminates (and is deterministic).
The denotation `[]` is an answer set.
For terminating queries, thetheorem is:
`(run* (staged sg)) ~=~ (run* (erase sg))`.

Another way:

```
top_staged(sg) = rg
[rg] = [erase(sg)]
```

`top_staged(sg)` produces a substitution and a program, and reifies it into an `rg`.

```
top_staged(sg) = reify(staged(sg))
```

## Definition of `staged(sg) = [(s,c)]`

```
// for concision, when we write staged
// we mean staged_x with staged_x for all recursive calls
// staged_f is when in surrounding fallback evaluation
// staged_t is when not in surrounding fallback evaluation (the default)

// we assume lazy containers (ferns?), and /++/ is interleaving, non-starving for any side

// need to define composition o:
// informally, s1 o s2
// for each var x, have x -> unify (s1 x) (s2 x) and fail globally if unifies fail.
// These compositions fail:
// ([x -> 3] o [x -> 2])
// ([x -> 3, y -> x] o [y -> 2])

// c is a list of constraints
// l is a list of code

staged(sg): lazy container of substitution and code pairs: [(s,c,l)]

staged(g(sg))
  staged(== t1 t2) = {
    s = unify t1 t2
    if s then [(s,[],[])] else []
  }
  staged(=/= t1 t2) = {
    c = solve(=/= t1 t2)
    if c then [(∅,c,[])] else []
  }
  etc.
  staged(conj g1 g2) = {
    ls1 = staged(g1)
    ls2 = staged(g2)
    [ s1 o s2, c, l1 ++ l2
      for all (s1,c1,l1) in ls1
        for all (s2,c2,l2) in ls2
          if s1 o s2
          let c = solve(s1 o s2, c1 ++ c2)
          if c]
  }
  staged(disj g1 g2) = staged(g1) /++/ staged(g2)
  staged(fresh1 v sg) = staged(sg[v := fresh_logic_var()])
  staged(fresh (tv ...) sg ...) // just a macro over fresh1
  staged(conde (sg ...) ...) // just a macro of disj of conjs
  staged(partial-apply t rname t ...) // omitted for now
  staged(finish-apply t rname t ...) // omitted for now

staged(later lg) = (success, [lg])
staged_t(gather sg) = (success, buildDisj(staged(sg)))
staged_t(fallback sg) =
  ls = staged_f(sg)
  if |ls|==0 then [] else if |ls|==1 staged_t(sg) else [(∅,[],erase(sg))]
// for nested fallback and gather cases
staged_f(fallback sg) = succeed
staged_f(gather sg) = succeed
staged(specialize-partial-apply t r t ...) // omitted for now

buildDisj(ls) = {
 (conde . [state-to-code(s,c,l) for all (s,c,l) in ls])
}

state-to-code((s,c,l)) =
  [for each binding (x,v) in s:
     yield (== x v)]
  ++
  [for each constraint c in c:
    constraint-to-code(c)]
  ++
  l

constraint-to-code((=/= a b)) = (=/= a b)
etc.

```
