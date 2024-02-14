#lang racket/base

(require "../../main.rkt"
         "../../test-check.rkt")

;; Runtime relation application in a runtime goal
(defrel (unify-5 x)
  (== x 5))
(test
 (run 1 (q) (unify-5 q))
 '(5))

;; Basic staging: `staged` and `later`.
(test
 (run 1 (q)
   (staged (later (== q 1))))
 '(1))

;; This should generate a trivial residual program.
(test
 (run 1 (q)
   (staged (== 1 1)))
 '(_.0))

;; `later` application of a runtime goal
(test
 (run 1 (q)
   (staged (later (unify-5 q))))
 '(5))

;; Staging within a runtime relation happens at definition-time,
;; and the relation can be called in runtime goals.
(defrel (unify-1 a)
  (staged
   (later (== a 1))))
(test
 (run 1 (q) (unify-1 q))
 '(1))

;; Staging-time branching. `staged` expects exactly one answer from its goal.
(test
 (run 1 (q)
   (staged
    (fresh (x)
      (== x 1)
      (conde
        [(== x 1)
         (later (== q 1))]
        [(== x 2)
         (later (== q 2))]))))
 '(1))

;; Staging-time fresh (and its conjunction behavior)
(test
 (run 1 (q)
   (staged
    (fresh (x y)
      (later (== q (cons x y)))
      (later (== x 1)))))
 '((1 . _.0)))

;; Generator goals have a staging-time body and can be applied in staging-time code.
(defrel/staged (gen-unify-5 x)
  (later (== x 5)))
(test
 (run 1 (q)
   (staged (gen-unify-5 q)))
 '(5))

;; It should be okay if there are no `later`s in a `staged`.
(test
 (run 1 (q)
   (staged
    (== q 1)))
 '(1))
