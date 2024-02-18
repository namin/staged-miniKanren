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

`[(top_staged sg)] ~=~ [(erase sg)]` if staging terminates successfully (and is deterministic).
The denotation `[]` is an answer set.
For terminating queries, the theorem is:
`(run* (staged sg)) ~=~ (run* (erase sg))`.

Another way:

```
top_staged(sg) = rg
[rg] = [erase(sg)]
```

`top_staged(sg)` produces a substitution and a program, and reifies it into an `rg`.

```
top_staged(sg) = state-to-code(staged(sg))
```

```
[[(run n (q) (staged sg))]] = {
  n = counter(initial_state)
  state' = inc_counter(initial_state)
  q' = var(n)
  sg' = substitute q' for q in sg
  singleton-stream(syntax) = capture(sg', state')
  syntax' = substitute q for q' in syntax
  [[(run n (q) syntax')]]
}


Theorem:
[[(run n (q) (staged sg))]] ~=~ [[(run n (q) erased(sg))]]

~=~: set of answers produced after reification
```

### Proof by structural induction on sg

Case (== t1 t2):
  Subcase 1:
     We evaluate ==, that gives us an updated state.
     We evaluate top-level capture, that generate syntax for the unifications.
     What it does depends on whether the term unifications refer to q or not.
  
  if s = unify t1 t2 succeeds then
    state-to-code(staged(sg)) == state-to-code([(s,[],[])]) =
    s produces a conjunction of unifications
    need it to be equivalent to to (== t1 t2)
    need to specify specify unstaged semantics
  
  Subcase 2: if ... fails: then staging fails. Vacuous.
  
Case (conj g1 g2):
  by induction:
    `[(top_staged g1)] ~=~ [(erase g1)]`
    `[(top_staged g2)] ~=~ [(erase g2)]`
    
    if composition of substitution succeeds
    if constraint solving succeeds
    
    staged(sg) = [s1 o s2, solve(s1 o s2, c1 ++ c2), l1 ++ l2]
    vs
    (erased(conj g1 g2)) = same semantics
    
    need to reason about composed substitution,
    turning that into unifications
    
    need to srength hypothesis to not go to top
    
    need to worry about reordering

## Unstaged Partial Apply

```
how unifies treat apply-rep: it ignores the fun position, otherwise structural.

[(residual-partial-apply t0 rname t ... proc)] =
  proc should take as many arguments as the second application of rname
  and returns a goal (a function from state to stream)
  t0 unifies with (apply-rep rname t ... proc)
[(partial-apply t0 rname t ...)] = t0 unifies with (apply-rep rname t ... <ignored>)
[(finish-apply t0 rname t ...)] = walk t0, and get a rep
three cases: rep is fresh, rep is an apply-rep without a proc, rep is an apply-rep with a proc
if rep has a proc:
then (proc t ...)
else if rep has no proc, but is an apply rep
apply rname with t0, the first-application args from rep and t ...
else // rep is fresh
we allocate the fresh variables for the first-application params
create an apply-rep struct that has the real name and that list
and unify t0 with this apply-rep
and then do apply rname, etc.
```

## Monadic Definition

Top-level entry point: `staged(sg) = rg`

Local thing: `stagedi(s) = \state -> stream<state>`

`state` is a staging state, with the `l` part of the store.

Need to be a stream because in fallback, want to fallback if there are at least two things, and what this to work regardless of whether there are infinite substreams at staging time. In the real interpreter, we almost always have things setup so that the branches are almost always cutoff by nested fallbacks. Symbol case with lookupo, eg.

Syntactic convenience:
`[s] state = stream<state>`

Spelling out the state:
(substitution+constraints sc, code l, counter n, list of touched variables t, falling-back f)

what is the datatype in the L part of the state?
it has a representation of the constructs of the language but also terms inside of that, which may contain logic variables.
in real implementation, syntax objects for the constructs, and normal miniKanren values for the terms.
We we use data tag to distinguish between == and the data parts.

```
[(== t1 t2)] state =
  // TODO: update the touched variables
  // in addition to the sc, returns variables that got extended
  sc = unify t1 t2 (SC(state))
  if sc then stream-singleton(update-SC(sc, state)) else fail

[(fresh (x) sg)] state = {
  n = counter(state)
  state' = inc-counter(state)
  x' = var(n)
  sg' = substitute x' for x in sg
  // assumes substitute does the right scoping of nested freshes
  [sg'] state'
}

[(conde ((g1 ...) ...))] =
// let's do disj instead

[(conj sg1 sg2)] state = stream-bind([sg1] state) [sg2]
[(disj sg1 sg2)] state = stream-append(([sg1] state), ([sg2] state))

[(later lg)] state = stream-singleton(add-update-L(lg, state))

[(gather sg)] state = stream-singleton(add-update-L(buildDisj(sg, state)))

[(fallback sg)] state@(sc, l, n, t, true)  = stream-singleton((sc, l, n, true))
[(fallback sg)] state@(sc, l, n, t, false) = 
  case take(2, [sg]((sc, l, n, t, true)) of
    []        => stream-empty
    [x]       => x
    otherwise => stream-singleton(add-update-L(erase(sg), (sc, l, n, false)))

// global environment env
// note: maybe leave out the recursive aspect, though it's not hard to write down
[(specialize-partial-apply t_rep r_name t_args ...)] state = {
  (defrel-partial/staged (_ tv_rep [tv1 ...] [tv2 ...]) sg) = lookup r_name in env
  sg' = substitute t_args for tv1 in sg
  // note: we fresh within capture in the impl, with some subtle point ensuing
  tv2' = fresh vars for each tv2, state' = inc++ counter state
  sg'' = substitute tv2' for tv2 in sg'
  sg''' = substitute t_rep for tv_rep in sg''
  singleton-stream(syntax) = capture(sg''', state'')
  syntax' = substitute tv2 for tv2' in syntax
  [(later (== t_rep APPLYREP(r_name (t_args ...) lambda(tv2 ...).syntax')))] state
}

// capture it takes a staged goal and a state and returns a stream of syntax after reflecting all the constraints and closing any free variables with a fresh binding
capture(sg, state) = {
  // capture without constraints
  n = counter(state)
  state' = empty-L(state)
  stream-bind ([sg] state') generate-syntax(n)
}

generate-syntax(n) state = {
 // counter tells us which variables are fresh inside or outside
 // touched variables tells us which variables got their values updated inside the goal
 t = touched(state)
 roots = filter t with variable counter <= n
 s = SUBST(state)
 Lroot = for each root, generate (== root (walk* v s))
 fresh_local_vars(n, Lroot ++ walk*(L(state), s))
}

fresh_local_vars(n, L) = {
 vars = find_all_local_vars(n, L)
 vars' = fresh identifiers for vars
 L' = substitute vars' for vars in L
 fresh(vars') conj*(L')
}

find_all_local_vars(n, L) = {
 // find all the variables in L and filter those that are local based on the counter n, so > n.
}

buildDisj(sg, state) = {
 list-of-syntaxes = take-all(capture(sg, state))
 (conde . list-of-syntaxes)
}

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

staged(later lg) = (success, [lg])
staged_t(gather sg) = (success, buildDisj(staged(sg)))
staged_t(fallback sg) =
  ls = staged_f(sg)
  if |ls|==0 then [] else if |ls|==1 staged_t(sg) else [(∅,[],erase(sg))]
// for nested fallback and gather cases
staged_f(fallback sg) = succeed
staged_f(gather sg) = succeed
staged(specialize-partial-apply t0 r t ...) =
we unify t0 with an apply-rep that has a proc that is a lambda with the result of staging the relation // TODO: not precise enough to be meaningful

buildDisj(ls) = {
 (conde . [state-to-code(s,c,l) for all (s,c,l) in ls])
}

// in the impl, we have a way to decide whether a variable is local to the dynamic extent of the code generation process
// make sure substitution is fully applied
state-to-code((s,c,l)) =
  (fresh (local-variable ...) // in real impl, need to substitute
  [for each binding (x,v) in s:
     yield (== x (walk* v s))
     if x is local]
  ++
  [for each constraint c in c:
    constraint-to-code((walk* c s))]
  ++
  (walk* l s))

constraint-to-code((=/= a b)) = (=/= a b)
etc.

```
