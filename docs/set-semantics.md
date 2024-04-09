Another attempt at semantics, with help from Jason. Three new ideas:

1. Pass the state through conjunction, but use sets for disjunction and set comprehensions wherever multiple results are transformed.
2. Handle variable allocations and variable freshening in generate-syntax via side conditions instead of via a counter.
3. Express nested fallback behavior via a substitution of succeed for the nested fallbacks, instead of via a parameter.

```
[(== t1 t2)] st =
  sc, vs = unify(t1, t2, (SC(st)))
  if sc then {update-SC(sc, state)} else {}

[[(disj gs1 gs2)]] st = gs1 st U gs2 st

[[(conj gs1 gs2)]] st = U st' \in gs1 st. gs2 st'

[[(fresh (x) gs)]] st = [[gs[x'/x]]] st  where x' \not\in (st, gs)

[[(later gl)]] st = add-L(gl, st)

[[(fallback gs)]] st =
  res = [[gs[succeed / (fallback _)]]] in
  if |res| <= 1 then res else {add-L(erase(gs), st)}

[[(gather gs)]] st = `(disj ,@capture(gs, st))

capture(gs, st) = 
  res = [[gs]] empty-L(st)
  U st' \in res . generate-syntax(gs, st, st')  when res is finite

generate-syntax(gs, st, st') =
 Ls = `(== ,x ,v) for x,v in SUBST(st'):
 local-vars = vars(st') - (vars(st) U vars(gs))
 `(fresh (,@local-vars) ,@(Ls ++ L(st')))
 ```