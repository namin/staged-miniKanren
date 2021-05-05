- [x] Use a reify tag for quasi instead of ad-hoc knowledge for each construct.
      Then `l==` becomes `(lift (== ,(reify a) ,(reify b)))`.

- [x] Investigate how/if unstaged evalo should reenter staged code.
      Share closure representation and calling convention.

- [x] Figure out why lambdas got demoted in synthesis
      because of the shared calling convention.
      Findings: No demotion, instead `list` got promoted because its closure in `initial-env` now had the right shape.

- [ ] Make `syn-hole` robust to multiple results in the generator,
      instead of sprinkling `non-varo`s.
      Might require systematic `run*` instead of `run 1` in generator.

- [ ] Unify representation of letrec-bound procedures and lambda-bound ones.

- [ ] Collect and fix examples of divergence during code generation due to infinite answers.
      Example: lambda arglist position could generate all number of arguments.

- [ ] Use gensyms for expand/unexpand tags.

- [ ] Find a test case that breaks the lack of expansion on the expr in the `u-eval-expo` `varo` case in the staged interpreter.
      Then add that `expand` call.

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
