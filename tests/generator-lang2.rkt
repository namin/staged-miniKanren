#lang racket/base

(require "../main.rkt"
         "../test-check.rkt")

(defrel (unify-5 x)
  (== x 5))

(test
 (run 1 (q) (unify-5 5))
 '(_.0))

(defrel (bar a)
  (staged
   (later
    (== a 1))))

(test
 (run 1 (q)
   (bar q))
 '(1))

(test
 (run 1 (q)
   (staged
    (later (unify-5 q))))
 '(5))

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

(test
 (run 1 (q)
   (staged
    (fresh (x y)
      (later (== q (cons x y))))))
 '((_.0 . _.1)))

(defrel/generator (gen-unify-5 x)
  (later
   (== x 5)))

(test
 (run 1 (q)
   (staged
    (gen-unify-5 q)))
 '(5))

;; It should be okay if there are no `later`s in a staged
(test
 (run 1 (q)
   (staged
    (== q 1)))
 '(1))

