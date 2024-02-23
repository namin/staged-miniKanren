#lang racket/base

(require "../main.rkt"
         "../test-check.rkt")


;; Failing tests for known bugs.


;; Check that we can't create a cyclic term via partial applications.
;; This should be forbidden by an occurs check, but it is so costly that
;; we currently omit it.
(defrel-partial/staged (cycle rec [env] [y])
  (== 1 1))
(test
 (run 1 (q)
   (fresh (r1 r2 env)
     (partial-apply r1 cycle env)
     ;; This unification should fail on the occurs check.
     (== env `((f . ,r1)))))
 '())

;; Check that we can't create a cyclic term via specialize-partial-apply
(test
 (run 1 (q)
   (staged
    (fresh (r1 r2 env)
      ;; This generates a later unification with an apply-rep that fails at
      ;; runtime due to the occurs check.
      (specialize-partial-apply r1 cycle env)
      (== env `((f . ,r1))))))
 '())