#lang racket/base

(require "../../main.rkt"
         "../../test-check.rkt")

(defrel-partial/staged (test-rel rep [y z] [x res])
  (fresh (yz)
    (== y z)
    (== yz `(,y ,z))
    (later (== res (cons x yz)))))

;; All at runtime, like unstaged interpretation of closures.
;;
;; Note that the different applications of the same partial
;; have different values for the later-stage arguments.
(test
 (run 1 (q)
   (fresh (c r1 r2)
     (partial-apply c test-rel 2 2)
     (finish-apply c test-rel 1 r1)
     (finish-apply c test-rel 4 r2)
     (== q `(,r1 ,r2))))
 '(((1 2 2) (4 2 2))))

;; All later, like fully-staged interpretation of closures
(test
 (run 1 (q)
   (staged
    (fresh (c r1 r2)
      (later (partial-apply c test-rel 2 2))
      (later (finish-apply c test-rel 1 r1))
      (later (finish-apply c test-rel 4 r2))
      (later (== q `(,r1 ,r2))))))
 '(((1 2 2) (4 2 2))))

;; Mixed stage, like with a staged closure called in runtime code
(defrel (like-callo c arg res)
  (finish-apply c test-rel arg res))
(test
 (run 1 (q)
   (staged
    (fresh (c r1 r2)
      (later (partial-apply c test-rel 2 2))
      (later (like-callo c 1 r1))
      (later (like-callo c 4 r2))
      (later (== q `(,r1 ,r2))))))
 '(((1 2 2) (4 2 2))))

;; A later partial-apply fails if the generator fails.
(test
 (run 1 (q)
   (staged
    (conde
      [(fresh (c)
         (specialize-partial-apply c test-rel 2 3)
         (later (== q 'branch-1)))]
      [(later (== q 'branch-2))])))
 '(branch-2))

;; Ensure that specializing a partial application doesn't commit any unifications within
;; to the state accidentally via set-var-val!
(defrel-partial/staged (unify-5 rep [x] [y])
  (== x 5))
(test
 (run 1 (q)
   (staged
    (fresh (x rep)
      (specialize-partial-apply rep unify-5 x)
     (== x 6)
      (== q x))))
 '(6))

;; Check that we can't create a cyclic term via partial applications
(defrel-partial/staged (cycle rec [env] [y])
  (== 1 1))
(test
 (run 1 (q)
   (staged
    ;; I want to test staging-time failure, but we don't have a way to catch the exception right
    ;; now. So the structure of this test is a bit of a hack.
    (conde
      [(fresh (r1 r2 env)
         (partial-apply r1 cycle env)
         ;; This unification should fail on the occurs check...
         (== env `((f . ,r1))))] 
      ;; ... and thus only this branch should succeed at staging time.
      [(== q 'branch-2)])))
 '(branch-2))
(defrel-partial/staged (equalo rep [a b] [c])
  (conde
    [(== a b) (later (== c #t))]
    [(=/= a b) (later (== c #f))]))

;; Right now this is an error. Should it be? Or should the generator use gather
;; or fallback if it should not be?
#;(run 1 (q)
  (staged
   (fresh (a b rep)
     (later (== rep (partial-apply equalo a b))))))
