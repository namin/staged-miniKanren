#lang racket/base

(require "../../private/internals.rkt"
         "../../test-check.rkt")

;; For now we need dynamic and staged versions of the relation; when we have stage
;; polymorphism we won't.

(define (test-rel-dyn y z x res)
  (fresh (yz)
    (== yz `(,y ,z))
    (== res (cons x yz))))

(define (test-rel-staged rep y z x res)
  (fresh (yz)
    (== yz `(,y ,z))
    (l== res (cons x yz))))

; Running forward; uses test-rel-staged to generate code that gets called at each apply-partial.
; Second-stage arguments (those arguments not given at lpartial-apply) may differ at different calls.
(test
  (run-staged 1 (q)
    (fresh (c r1 r2)
      (lpartial-apply c ((test-rel-staged test-rel-dyn) (2 3) (_ _)))
      (apply-partial c ((test-rel-staged test-rel-dyn) (_ _) (1 r1)))
      (apply-partial c ((test-rel-staged test-rel-dyn) (_ _) (4 r2)))
      (l== q (list r1 r2))))
  '(((1 2 3) (4 2 3))))

(test
  (run-staged 1 (q)
    (fresh (c r1 r2)
      (lpartial-apply c ((test-rel-staged test-rel-dyn) (2 3) (_ _)))
      (lapply-partial c ((test-rel-staged test-rel-dyn) (_ _) (1 r1)))
      (lapply-partial c ((test-rel-staged test-rel-dyn) (_ _) (4 r2)))
      (l== q (list r1 r2))))
  '(((1 2 3) (4 2 3))))

(test
  (run-staged 1 (q)
    (fresh (c r1 r2)
      (partial-apply c ((test-rel-staged test-rel-dyn) (2 3) (_ _)))
      (apply-partial c ((test-rel-staged test-rel-dyn) (_ _) (1 r1)))
      (apply-partial c ((test-rel-staged test-rel-dyn) (_ _) (4 r2)))
      (l== q (list r1 r2))))
  '(((1 2 3) (4 2 3))))

; the lifted unification from test-rel-staged fails when (5 6 7) is given for res at apply-partial
(test
  (run-staged 1 (q)
    (fresh (c)
      (lpartial-apply c ((test-rel-staged test-rel-dyn) (2 3) (_ _)))
      (lapply-partial c ((test-rel-staged test-rel-dyn) (_ _) (q '(5 6 7))))))
  '())

; the first apply-partial unifies the shared / first-stage argument v with 3, so q gets 3.
(test
  (run-staged 1 (q)
    (fresh (c v)
      (lpartial-apply c ((test-rel-staged test-rel-dyn) (2 v) (_ _)))
      (lapply-partial c ((test-rel-staged test-rel-dyn) (_ _) (1 '(1 2 3))))
      (lapply-partial c ((test-rel-staged test-rel-dyn) (_ _) (4 `(4 2 ,q))))))
  '(3))

; fails because the two apply-partials try to unify v with different values
(test
  (run-staged 1 (q)
    (fresh (c v)
      (lpartial-apply c ((test-rel-staged test-rel-dyn) (2 v) (_ _)))
      (lapply-partial c ((test-rel-staged test-rel-dyn) (_ _) (1 '(1 2 3))))
      (lapply-partial c ((test-rel-staged test-rel-dyn) (_ _) (4 '(4 2 5))))))
  '())

; uses test-rel-dyn to run backwards; no staging, a and b args are unified with 1 and 2 at lpartial-apply
(test
  (run-staged 1 (q)
    (fresh (c)
      (lapply-partial c ((test-rel-staged test-rel-dyn) (_ _) (1 q)))
      (lpartial-apply c ((test-rel-staged test-rel-dyn) (2 3) (_ _)))))
  '((1 2 3)))

; fails when arg unification fails at lpartial-apply
(test
  (run-staged 1 (q)
    (fresh (c)
      (lapply-partial c ((test-rel-staged test-rel-dyn) (_ _) (1 '(1 2 3))))
      (lpartial-apply c ((test-rel-staged test-rel-dyn) (5 6) (_ _)))))
  '())

; fails when to partials with ununifiable arguments are unified
(test
  (run-staged 1 (q)
    (fresh (c)
      (lpartial-apply c ((test-rel-staged test-rel-dyn) (1 2) (_ _)))
      (lpartial-apply c ((test-rel-staged test-rel-dyn) (3 4) (_ _)))))
  '())

; fails because the dynamic evaluation in the two apply-partials force the (unseen)
; first-stage argument y to different values
(test
  (run-staged 1 (q)
    (fresh (c v)
      (lapply-partial c ((test-rel-staged test-rel-dyn) (_ _) (1 '(1 2 3))))
      (lapply-partial c ((test-rel-staged test-rel-dyn) (_ _) (4 '(4 2 5))))))
  '())
