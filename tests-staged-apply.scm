(load "staged-load.scm")
(load "staged-apply.scm")

;; For now we need dynamic and staged versions of the relation; when we have stage
;; polymorphism we won't.

(define (test-rel-dyn y z x res)
  (fresh (yz)
    (== yz `(,y ,z))
    (== res (cons x yz))))

(define (test-rel-staged y z x res)
  (fresh (yz)
    (== yz `(,y ,z))
    (l== res (cons x yz))))

; Running forward; uses test-rel-staged to generate code that gets called at each apply reified.
; Second-stage arguments (those arguments not given at reify-call) may differ at different calls.
(test
  (run-staged 1 (q)
    (fresh (c r1 r2)
      (reify-call c ((test-rel-staged test-rel-dyn) (2 3) (_ _)))
      (apply-reified c ((test-rel-staged test-rel-dyn) (_ _) (1 r1)))
      (apply-reified c ((test-rel-staged test-rel-dyn) (_ _) (4 r2)))
      (l== q (list r1 r2))))
  '(((1 2 3) (4 2 3))))

; the lifted unification from test-rel-staged fails when (5 6 7) is given for res at apply-reified
(test
  (run-staged 1 (q)
    (fresh (c)
      (reify-call c ((test-rel-staged test-rel-dyn) 2 3 _))
      (apply-reified c ((test-rel-staged test-rel-dyn) _ _ q (5 6 7)))))
  '())

; the first apply-reified unifies the shared / first-stage argument v with 3, so q gets 3.
(test
  (run-staged 1 (q)
    (fresh (c v)
      (reify-call c ((test-rel-staged test-rel-dyn) 2 v _ _))
      (apply-reified c ((test-rel-staged test-rel-dyn) _ _ 1 '(1 2 3)))
      (apply-reified c ((test-rel-staged test-rel-dyn) _ _ 4 `(4 2 ,q)))))
  '(3))

; fails because the two apply-reifieds try to unify v with different values
(test
  (run-staged 1 (q)
    (fresh (c v)
      (reify-call c ((test-rel-staged test-rel-dyn) 2 v _ _))
      (apply-reified c ((test-rel-staged test-rel-dyn) _ _ 1 '(1 2 3)))
      (apply-reified c ((test-rel-staged test-rel-dyn) _ _ 4 '(4 2 5)))))
  '())

; uses test-rel-dyn to run backwards; no staging, a and b args are unified with 1 and 2 at reify-call
(test
  (run-staged 1 (q)
    (fresh (c)
      (apply-reified c ((test-rel-staged test-rel-dyn) _ _ 1 q))
      (reify-call c ((test-rel-staged test-rel-dyn) 2 3 _ _))))
  '((1 2 3)))

; fails when arg unification fails at reify-call
(test
  (run-staged 1 (q)
    (fresh (c)
      (apply-reified c ((test-rel-staged test-rel-dyn) _ _ 1 '(1 2 3)))
      (reify-call c ((test-rel-staged test-rel-dyn) 5 6 _ _))))
  '())

; fails when to reified calls with ununifiable arguments are unified
(test
  (run-staged 1 (q)
    (fresh (c)
      (reify-call c ((test-rel-staged test-rel-dyn) 1 2 _ _))
      (reify-call c ((test-rel-staged test-rel-dyn) 3 4 _ _))))
  '())

; fails because the dynamic evaluation in the two apply-reifieds force the (unseen)
; first-stage argument y to different values
(test
  (run-staged 1 (q)
    (fresh (c v)
      (apply-reified c ((test-rel-staged test-rel-dyn) _ _ 1 '(1 2 3)))
      (apply-reified c ((test-rel-staged test-rel-dyn) _ _ 4 '(4 2 5)))))
  '())
