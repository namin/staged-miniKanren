#lang racket/base

(require "generator-lang2.rkt"
         "test-check.rkt")

(defrel (foo a)
  (== a 5))

(test
 (run 1 (q) (foo 5))
 '(_.0))

(test
 (run 1 (q)
   (staged
    (later
     (== q 1))))
 '(1))

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
      (later
       (fresh ()
         (== x q)
         (conde
          [(now (symbolo x)) (== y 1)]
          [(now (numbero x)) (== y 1)]))))))
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
      (later
       (fresh ()
         (conde
          [(now (symbolo x)) (== y 1)]
          [(now (numbero x)) (== y 1)]))))))
 '(_.0 _.0))

(test
 (run 3 (q)
   (staged
    (fresh (x y)
      ;;(== q (cons x y))
      (later
       (fresh ()
         (conde
           [(now (symbolo x))
            (conde
              [(symbolo x)
               (== y 1)]
              [(numbero x)
               (== y 2)])]
          [(now (numbero x)) (== y 1)]))))))
 '(_.0 _.0))

;; TODO: should be okay if there are no `later`s in a staged
(test
 (run 1 (q)
   (staged
    (== q 1)))
 '(1))

;; TODO: this fails for two reasons.
;; - conde: bad syntax in: (conde () ())
;; - we don't capture substitution extensions in conde
(test
 (run 2 (q)
   (staged
    (fresh (x y)
      (== q x)
      (later
       (conde
        [(now (== x 1))]
        [(now (== x 2))])))))
 '(1 2))

(test
 (run 2 (q)
   (staged
    (fresh (x y)
      (== q x)
      (later
       (conde
        [(now (== x 1)) (== y 1)]
        [(now (== x 2)) (== y 1)])))))
 '(1 2))

;; TODO: this one is even harder! There's no way to tell at the end of capture-later
;; that the value of x will end up being relevant to the later stage.
;;
;; I think we have to reflect all store / substitution extensions and then do dead
;; code elimination on the final program.
(test
 (run 2 (q)
   (staged
    (fresh (x y)
      (later
       (conde
        [(now (== x 1)) (== 1 1)]
        [(now (== x 2)) (== 1 1)]))
      (== q x))))
 '(1 2))
