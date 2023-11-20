# Semantics

## Staging Correctness Theorem

```
erase(sg): fix(g)
erase(g(sg)) = g(erase(sg))
erase(later(lg)) lg
erase(fallback sg) = erase(sg)
erase(specialize-partial-apply t r t ...) = (partial-apply t r t ...)
```

`[(staged sg)] ~=~ [(erase sg)]` if staging terminates (and is deterministic).
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

## Definition of `staged(sg) = rg`

```
istaged(sg) = intermediary thing
f(intermediary thing) = staged(sg)
```

`intermediary thing`: like the denotational semantics with tupled up code fragment
but need to think about divergence / infinite computation

- what's the inductive invariant that we need for `istaged`
- what properties of a denotional semantics is that going to depend on to prove the invariant

in the restricted case, and only accounting for terminating and deterministic, there should be a pretty easy to prove.
as soon as we start relaxing some of the conditions, then challenges.

``{f : A → D | f (t1) = f (t2)} [UnifyD ]``

set of (r,c)
vs
((set of r), c)

what's a result of a staging time disjunction?

we need a notion of top-level to filter out non-determinism.
so:
top(staged(sg))
staged(sg) = intermediary thing
top(intermediary thing) = ((set of r), c)
intermediary thing = (streams of (s,c)).
constraints other than unifications -- seems like a simplification
do we have to worry about non-terminating streams?
in the absence of fallback, everything has to terminate
so let's imagine we're doing the semantics for a simpler system without fallback
then any infinite stream would lead to failure, so in the absence of fallback, we could get away with a definition of search without interleaving
so let's go with list (s,c)


```
staged(g(sg))
  staged(== t1 t2) = {
    s = unify t1 t2
    if s then [(s,[])] else []
  }
  etc.
  staged(conj g1 g2) = {
    l1 = staged(g1)
    l2 = staged(g2)
    [ s1 o s2, c1 ++ c2
      for all (s1,c1) in l1
      for all (s2,c2) in l2
      if s1 o s2 ]
  }
  staged(disj g1 g2) = staged(g1) ++ staged(g2)
  staged(fresh (tv ...) p ...)
  staged(conde (p ...) ...)
  staged(partial-apply t rname t ...) // omitted for now
  staged(finish-apply t rname t ...) // omitted for now

staged(later lg) = (success, [lg])
staged(gather sg) = (success, buildDisj(staged(sg)))
staged(fallback sg) = // assume we don't have infinite answers for now
  l = parameterize(in-surrounding-fallback-eval?)(staged(sg))
  if |l|==0 then [] else if |l|==1 staged(sg) else [((),erase(sg))]
parameterize(in-surrounding-fallback-eval?)(staged(fallback sg)) =
  l = staged(sg)
  if |l|==0 then [] else [((),())]
staged(specialize-partial-apply t r t ...) // omitted for now

buildDisj(l) = {
 (conde . [reify(s,c) for all (s,c) in l])
}

for
[
(x -> 1, [(== x 2)])
(x -> 2, [(== x y)])
(y -> 3, [])
]
we could generate
[(== x 1) (== x 2)],
[(== x 2) (== x y)],
[(== y 3)]
but we want to be more efficient if x or y are not used outside of here.
In reality, we are more efficient than this (tracking allocation of variables), but we can skip this for now.
(but what if none of the code refers to y, but maybe some code outside refers to y)

top(l) = {
 [(s,c)] = l // must be deterministic
 reify(s,c)
}

// QUESTION: we want to add a fresh for all variables used in s and c that aren't bound further out
//           or in another words maybe, that were allocated during the staging
//           we can make what we have right now correct by wrapping a fresh around all variables in top
//           but we need something more precise for partial application
reify(s,c) = {
 [(== a b) for a,b in s] ++ c
}
```

Note: this semantics convey something though it dodges all the difficult cases, including scoping.


### Fallback

Fallback takes a staging-time goal. If evaluating the staging-time goal produces exactly one answer, that's what we get.
If it produces two or more answers, then we generate an invocation to run-time code produces by erasing sg.
If it produces zero answers, then it fails.

```
staged(fallback sg) = {
  l = staged(sg)
  n = len(l)
  switch n {
    case 0 => []
    case 1 => l
    case _ => erased(sg)
  }
}
```

The difficulty is what happens when you have nested fallbacks and non-termination.

TODO: next step: deal with non-termination properly.


## Old

```
// OPEN QUESTION 1: what about the non-termination aspect of everything
staged(g(sg))
  staged(=/= t1 t2) = (like in paper, [])
  etc.
  staged(conj g1 g2) = {
    rc1 = staged(g1)
    rc2 = staged(g2)
    for all (r1,c1) in rc1:
      for all (r2,c2) in rc2:
        yield (r1 /\ r2, c1 ++ c2)
  }
  // OPEN QUESTION 2: for all, for all infinite sets is iffy
  staged(disj g1 g2) = staged(g1) \/ staged(g2)
  staged(fresh (tv ...) p ...)
  staged(conde (p ...) ...)
  staged(partial-apply t rname t ...) // omitted for now
  staged(finish-apply t rname t ...) // omitted for now

staged(later lg) = (success, [lg])
staged(gather sg) = (success, buildDisj(staged(sg)))
// OPEN QUESTION 3: buildDisj turns a representing function back into syntax -- doesn't seem like a good match for the representation
staged(fallback sg) // omitted for now
staged(specialize-partial-apply t r t ...) // omitted for now
```

How fine-grained do we want the semantics of staging to be?
Do we want a version that uses substitution like the real implementation does, instead of representing function? What aspects of the real system as opposed to the denotional semantics are useful to model?

Nada's hunch: the buildDisj reification will dictate the granularity.
