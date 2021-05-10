- [x] Use a reify tag for quasi instead of ad-hoc knowledge for each construct.
      Then `l==` becomes `(lift (== ,(reify a) ,(reify b)))`.

- [x] Investigate how/if unstaged evalo should reenter staged code.
      Share closure representation and calling convention.

- [x] Figure out why lambdas got demoted in synthesis
      because of the shared calling convention.
      Findings: No demotion, instead `list` got promoted because its closure in `initial-env` now had the right shape.

- [x] Remember constraints in the generated code.

- [x] Done only for staged cases.
      Refactor the application cases to be consolidated into one, and check the `varo rands` only once.

- [x] Use `gensym`s for `expand`/`unexpand` tags.

- [x] Moot. Also refactor `sym` to use `gensym`.

- [x] Refactor `sym` to use `unexpand` instead, since the code is so similar.

- [x] Find a test case that breaks the lack of expansion on the expr in the `u-eval-expo` `varo` case in the staged interpreter.
      Then add that `expand` call.

- [ ] Collect and fix examples of divergence during code generation due to infinite answers. In particular:

    + [x] lambda arglist position could generate all number of arguments.
    + [x] (Crudely.) Pattern matching.
    + [x] Define `ground-spineo` and use it instead of `varo` for `rands` case.
    + [x] Define `ground-paramso` for lambda parameter case.
          Need fully ground because we want `lookupo` to be deterministic.
    + [x] Non-determinism when hole in application position.
          See todo "non-determinism" test in tests.
    + [ ] `letrec` (clauses, bindings, lambda formals).
    + [ ] Variadic `and`.
    + [ ] Variadic `or`.
    + [ ] Make `match` ground checking less crude.
          Scrutinies need to be ground.
          Spine of clause list needs to be ground.

- [ ] Shake the generator by sampling `u-eval-expo`.

- [ ] Examine the `eval-expo #f` for rator evaluation.
      Possible test: `(let ([f ,e]) (f 5))`.
      Also re-consider whether `stage?` should be `#t` when lifting to `u-eval-expo` when `varo`.

- [ ] Debug last slow test of Will.

- [ ] Think about: Just as
      staging is partial evaluation with manual annotations instead of heuristics,
      relational staging is partial deduction with manual annotations instead of heuristics.

- [ ] @michaelballantyne will write a demo of syntactic lift.

- [ ] Revive `staged-regexp` application.

- [ ] Revive `fuzzing` application.

- [ ] Unify representation of letrec-bound procedures and lambda-bound ones.

- [ ] How can staged interpreter be as close as possible to unstaged interpreter?
      In particular, the `letrec` and `lambda` cases are different.
      For `letrec`, we might need tabling.

- [ ] Make `syn-hole` robust to multiple results in the generator,
      instead of sprinkling `non-varo`s.
      Might require systematic `run*` instead of `run 1` in generator.

- [ ] Consider whether refactoring of application cases should also be done for non-staged cases.
      In general, probably need to make the result of deferred closures dynamic.

- [ ] Consider low-level optimizations in the generated code like getting rid of unnecesarry structures in unification.

- [ ] Consider this example. There is a blow up of spurious results in the second stage.
      Furthermore, we get non-determinism, which doesn't mesh well with `run 1`.
      Consider the work on partial deduction to get ideas to alleviate these issues.
      Or maybe a construct `conj-do-cartestian-product`.
```
(define-relation (foo x y)
(conj
  (conde
    [(== x 1)
     (l== y 1)]
    [(== x 1)
     (l== y 2)]
    [(== x 2)
     (l== y 3)])
  (conde
    [(l== y 1)]
    [(l== y 2)]
    [(l== y 3)])))

(foo 1 z)
```

- [ ] Devise a much more pleasant interface for running and staging than `gen`.

Replacement for `gen`:

```
(define-staged-relation (appendo xs ys zs)
 (eval-staged
   `(letrec ((append
               (lambda (xs ys))))
       (append ',xs ',ys))
   zs))
(run* (q) (appendo q '(b) '(a b)))
```

```
(define-relation (appendo xs ys zs)
 (eval-staged
   `(letrec ((append
               (lambda (xs ys))))
       (append ',xs ',ys))
   zs))
(run-staged* (q) (appendo q '(b) '(a b)))
```

```
(run-staged 1 (q)
 (evalo-staged
   `(letrec ((append
                 (lambda (xs ys)
                   (if (null? xs) ,q
                       (cons (car xs) (append (cdr xs) ys))))))
         (append '(1 2) '(3 4)))
  '(1 2 3 4)))
```
