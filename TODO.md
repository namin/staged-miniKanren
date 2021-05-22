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

- [x] Collect and fix examples of divergence during code generation due to infinite answers, and non-determinism in general. In particular:

    + [x] lambda arglist position could generate all number of arguments.
    + [x] (Crudely.) Pattern matching.
    + [x] Define `ground-spineo` and use it instead of `varo` for `rands` case.
    + [x] Define `ground-paramso` for lambda parameter case.
          Need fully ground because we want `lookupo` to be deterministic.
    + [x] Non-determinism when hole in application position.
          See todo "non-determinism" test in tests.
    + [x] `letrec` (clauses, bindings, lambda formals).
    + [x] (Already done by app consolidation.) Variadic `and`.
    + [x] (Ditto.) Variadic `or`.
    + [x] Make `match` ground checking less crude.
          Scrutinies need to be ground.
          Spine of clause list needs to be ground.
    + [x] Fix the "non-determinism" test.
          Somehow, we're getting true divergence on application with two arguments and unground function.
          Was due to `prim-id` being a variable.
          Not true divergence, but n=10.

- [x] This example should not fail.
      It fails because of the `non-varo` on `prim-id` to solve the non-determinism above.
      It seems like there should be a cut to `dynamic` at the point where eval of `rator` doesn't constraint the `rator` value.
    + [x] This example already works.
```
(run-staged 1 (q)
 (staged-evalo `(,q (list 1 2)) 1)
 (l== q 'car))
```

- [ ] Shake the generator by sampling `u-eval-expo`.

- [ ] Run proper benchmarks comparing staged and unstaged programs.

- [ ] Examine the `eval-expo #f` for rator evaluation.
      Possible test: `(let ([f ,e]) (f 5))`.
      Also re-consider whether `stage?` should be `#t` when lifting to `u-eval-expo` when `varo`.

- [ ] Debug last slow test of Will.

- [ ] Think about: Just as
      staging is partial evaluation with manual annotations instead of heuristics,
      relational staging is partial deduction with manual annotations instead of heuristics.

- [ ] @michaelballantyne will write a demo of syntactic lift.

- [ ] For showing off synthesis it would be interesting to have some examples that use really big ground contexts;
      lots of helpers, say.

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

- [x] Devise a much more pleasant interface for running and staging than `gen`.
    + [x] `run-staged`.
    + [x] `run-stage` with multiple query variables.
    + [x] `run-staged*`.
    + [x] `defined-staged-relaion`.
        * ```(define-staged-relation (test e) (staged-evalo `(cons ,e '()) '(5)))```
        * it'd do the query ```(run 100 (e) (staged-evalo `(cons ,e '()) '(5)))```
        * you get back some answer for e, call it <ans>
        * then you compile to ```(define-relation (test e) (== e <ans>) <the-generated-code>)```
        * `gen` knew about the evaluator and its environment
        * `define-staged-relation` is not meant to know about that at all!
          deals with arbitrary staged-mk goals in its body
          knows nothing of evalo
    + [x] Convert some of the bench tests to use the new interface.

- [ ] Make `evalo` stage polymorphic so that it can be instantiated to `evalo-staged` or `evalo-unstaged` (`u-evalo`).

- [ ] Compose with pink.
