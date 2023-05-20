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
 (run 2 (q)
   (staged
    (fresh (x y)
      (later (== x q))
      (gather
       (conde
         [(symbolo x) (later (== y 1))]
         [(numbero x) (later (== y 1))])))))
 '((_.0 $$ (sym _.0)) (_.0 $$ (num _.0))))

(test
 (run 1 (q)
   (staged
    (fresh (x y)
      (later (== q (cons x y))))))
 '((_.0 . _.1)))

;; TODO: this generates code for the symbolo and numbero even though they aren't for a variable that's relevant at runtime
(test
 (run 2 (q)
   (staged
    (fresh (x y)
      (gather
       (conde
         [(symbolo x) (later (== y 1))]
         [(numbero x) (later (== y 1))])))))
 '(_.0 _.0))

(test
 (run 3 (q)
   (staged
    (fresh (x y)
      (gather
       (conde
         [(symbolo x)
          (conde
            [(later (symbolo x))
             (later (== y 1))]
            [(later (numbero x))
             (later (== y 2))])]
         [(numbero x) (later (== y 1))])))))
 '(_.0 _.0))


(defrel/generator (gen-unify-5 x)
  (later
   (== x 5)))

(test
 (run 1 (q)
   (staged
    (gen-unify-5 q)))
 '(5))


;; TODO: should be okay if there are no `later`s in a staged
(todo "no later staged should be OK"
      (run 1 (q)
        (staged
         (== q 1)))
      '(1))

;; TODO: this fails for two reasons.
;; - conde: bad syntax in: (conde () ())
;; - we don't capture substitution extensions in conde
(todo "now in later conde bug"
      (run 2 (q)
        (staged
         (fresh (x y)
           (== q x)
           (gather
            (conde
              [(== x 1)]
              [(== x 2)])))))
      '(1 2))

(todo "now in later conde bug"
      (run 2 (q)
        (staged
         (fresh (x y)
           (== q x)
           (gather
            (conde
              [(== x 1) (later (== y 1))]
              [(== x 2) (later (== y 1))])))))
      '(1 2))

;; TODO: this one is even harder! There's no way to tell at the end of capture-later
;; that the value of x will end up being relevant to the later stage.
;;
;; I think we have to reflect all store / substitution extensions and then do dead
;; code elimination on the final program.
(todo "now in later conde bug"
      (run 2 (q)
        (staged
         (fresh (x y)
           (gather
            (conde
              [(== x 1) (later (== 1 1))]
              [(== x 2) (later (== 1 1))]))
           (== q x))))
      '(1 2))
