#lang racket/base

(require "../../staged-interp.scm" "../../all.rkt")

;; I had hoped to illustrate the issues with out-of-order execution of partial
;; applications with an example like the following:
#;(run 2 (which-one)
  (fresh (fv) 
    (evalo-staged/env-exts `(f (cons 1 2)) '(f) (list fv) '(1 . 2))

    (conde
      [(staged (evalo-staged `(lambda (x) (cdr x)) fv)) (== which-one 1)]
      [(staged (evalo-staged `(lambda (x) 5) fv)) (== which-one 2)]
      [(staged (evalo-staged `(lambda (x) x) fv)) (== which-one 3)]
      [(staged (evalo-staged `(lambda (x) (cons 1 (cdr x))) fv)) (== which-one 4)]
      [(staged (evalo-staged `(lambda (x) (cons 1 3)) fv)) (== which-one 5)])))
;; However, this query doesn't come back after a very long time.
;;
;; So, what's wrong?

;; Similar to this trivial example-based synthesis in the interpreter...
(test
 (run 1 (q)
   (evalo-staged
    `(letrec ([f (lambda (x) ,q)])
       (list (f (cons 1 2))
             (f (cons 1 3))))
    (list 2 3)))
 '((cdr x)))
;; ...it seems like we should be able to do the same thing like this:
#;(run 2 (fv)
  (evalo-staged/env-exts `(f (cons 1 2)) '(f) (list fv) 2)
  (evalo-staged/env-exts `(f (cons 1 3)) '(f) (list fv) 3))
;; It takes a very long time, but eventually comes back with:
#;'((struct prim . cdr) (struct closure #s(apply-rep eval-apply ((#((unbound) (scope) 556)) (#((unbound) (scope) 1911) #((unbound) (scope) 556)) ((#((unbound) (scope) 1911) val struct prim . cdr) . #((unbound) (scope) 25406))) doesnt-matter)))

;; Why does it take so long?

;; When fv is fresh, nothing defines or restricts the environment of the closure.
;; So it guesses a bunch of environment variations instead of interesting terms.
#;(run 300 (body)
  (fresh (fv x p env)
    (evalo-staged/env-exts `(f (cons 1 2)) '(f) (list fv) 2)
    (== `(struct closure ,p) fv)
    (partial-apply p eval-apply x body env)))

;; If we require fv to be a closure with environment initial-env, we start guessing reasonable terms
;; and can solve the simple synthesis problem.
(test
 (run 1 (body)
   (fresh (fv x p)
     (== `(struct closure ,p) fv)
     (partial-apply p eval-apply x body initial-env)
    
     (evalo-staged/env-exts `(f (cons 1 2)) '(f) (list fv) 2)
     (evalo-staged/env-exts `(f (cons 1 3)) '(f) (list fv) 3)))
 '(((cdr _.0) $$ (=/= ((_.0 cdr))) (sym _.0))))

;; Why are we getting instantiation of the environment at all in cases where we don't look up in it, like a literal reference?
;;   Options of (lambda (x) ...) vs (lambda x ...)
;;   The not-in-envo for quote can instantiate the environment in arbitrary ways.
;;      Would be super cool if this could be a lazy constraint of some sort!
;;      This is not a problem when the environment is length-instantiated. But since it's not, we can keep guessing!


;; Presumably my original query would work given infinite time, too, but it takes forever
;; for it to start to consider anything other than quotes and environment references.


;; Once we set the env to the initial env, matching up with staged results works fine.
(test
 (run 2 (which-one)
   (fresh (fv x p body q)
     (== `(struct closure ,p) fv)
     (partial-apply p eval-apply x body initial-env)
    
     (evalo-staged/env-exts `(f (cons 1 2)) '(f) (list fv) '(1 . 2))

     (conde
       [(staged (evalo-staged `(lambda (x) (cdr x)) fv)) (== which-one 1)]
       [(staged (evalo-staged `(lambda (x) 5) fv)) (== which-one 2)]
       [(staged (evalo-staged `(lambda (x) x) fv)) (== which-one 3)]
       [(staged (evalo-staged `(lambda (x) (cons 1 (cdr x))) fv)) (== which-one 4)]
       [(staged (evalo-staged `(lambda (x) (cons 1 3)) fv)) (== which-one 5)])))
 '(3 4))
