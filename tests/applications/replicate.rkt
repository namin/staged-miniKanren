#lang racket/base

(require "../../main.rkt" "../../test-check.rkt")

(defrel-partial/staged (replicate rep [n] [l res])
  (gather
   (conde
     [(== l '())
      (== res '())]
     [(fresh (a d res-rec)
        (== l (cons a d))
        (cons-n n a res-rec res)
        (later (finish-apply rep replicate d res-rec)))])))

(defrel/staged (cons-n n v l res)
  (fallback
   (conde
     [(== n 'Z)
      (== l res)]
     [(fresh (n-1 rec-res)
        (== n `(S ,n-1))
        (== res (cons v rec-res))
        (cons-n n-1 v l rec-res))])))


;; Runtime-only test of the cons-n helper
(test
 (run 1 (q)
   (fresh (rep)
     (cons-n '(S (S (S Z))) 1 '(2 2 2 3 3 3) q)))
 '((1 1 1 2 2 2 3 3 3)))

;; Runtime-only test of replicate
(test
 (run 1 (q)
   (fresh (rep)
     (partial-apply rep replicate '(S (S (S Z))))
     (finish-apply rep replicate '(1 2 3) q)))
 '((1 1 1 2 2 2 3 3 3)))

;; Basic staged
(test
 (run 1 (q)
   (staged
    (fresh (rep)
      (specialize-partial-apply rep replicate '(S (S (S Z))))
      (later (finish-apply rep replicate '(1 2 3) q)))))
 '((1 1 1 2 2 2 3 3 3)))

;; Partially-staged: will replicate each element at least twice, but perhaps more.
;; The generated code has two unfolded `cons`es.
;; This relies on the fallback to terminate at staging-time.
(test
 (run 3 (q)
   (staged
    (fresh (rep n)
      (specialize-partial-apply rep replicate `(S (S ,n)))
      (later (finish-apply rep replicate '(1 2 3) q)))))
 '((1 1 2 2 3 3) (1 1 1 2 2 2 3 3 3) (1 1 1 1 2 2 2 2 3 3 3 3)))

(generated-code)