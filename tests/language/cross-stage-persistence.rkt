#lang racket/base

(require "../../main.rkt"
         "../../test-check.rkt")

;; `x` is connected to the query variable, so we must remember staging-time constraints
;; in order to reflect them in the runtime output.
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

;; Even though `x` is never connected to the query variables we must remember the
;; staging-time constraint on `x` in order to filter branches at runtime.
(test
 (run* (q)
   (staged
    (gather
     (fresh (x)
       (symbolo x)
       (conde
         [(later (symbolo x))
          (later (== q 1))]
         [(later (numbero x))
          (later (== q 2))])))))
 '(1))

;; Regression test: assignment to a previously-constrained variable results in an
;; empty constraint record. Make sure that constraint reflection doesn't break.
(test (run* (q)
        (staged
         (fresh (x )
           (symbolo x) (== x 'a) (later (== x q)))))
      '(a))

;; However, when a constraint applies to a variable that is not relevant to runtime,
;; we'd like to avoid reflecting runtime code for it.
;;
;; TODO: this currently generates code for the symbolo and numbero even though they
;; aren't for a variable that's relevant at runtime
(test
 (run* (q)
   (staged
    (fresh (x y)
      (gather
       (conde
         [(symbolo x) (later (== y 1))]
         [(numbero x) (later (== y 1))])))))
 '(_.0 _.0))

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

;; Like the above, but illustrates the substitution extension problem
;; without the other bug.
(test (run 2 (q)
        (staged
         (fresh (x y)
           (later (== q x))
           (gather
            (conde
              [(== x 1) (later (== y 1))]
              [(== x 2) (later (== y 1))])))))
      '(1 2))


;; This one is even harder! There's no way to tell at the end of capture-later
;; that the value of x will end up being relevant to the later stage.
;;
;; This works now because we save extensions for all variables created before
;; the capture-later, regardless of whether they have yet been shown to be relevant
;; to runtime. That may in general generate constraints that aren't actually needed
;; at runtime, but in practice it seems to work okay.
(test (run 2 (q)
        (staged
         (fresh (x y)
           (gather
            (conde
              [(== x 1) (later (== 1 1))]
              [(== x 2) (later (== 1 1))]))
           (== q x))))
      '(1 2))

