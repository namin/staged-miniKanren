#lang racket/base

(require "../../all.rkt")

;; when there are two branches that succeed, generate a conde with two branches
(test
 (length
  (run* (q)
    (staged
     (fresh (x)
       (gather
        (conde
          ((== x 1) (later (== q 'branch-1)))
          ((== x 2) (later (== q 'branch-2)))))))))
 2)

;; when gather captures multiple successes, a surrounding fallback should
;; not trigger
(test
 (length
  (run* (q)
    (staged
     (fallback
      (later (== q 'fallback))
      (fresh (x)
        (gather
         (conde
           ((== x 1) (later (== q 'branch-1)))
           ((== x 2) (later (== q 'branch-2))))))))))
 2)

;; when some branches succeed and some fail, the constructed goal has
;; branches for those that succeeded.
(test
 (run* (q)
   (staged
    (fresh (x)
      (== x 2)
      (gather
       (conde
         ((== x 1) (later (== q 'branch-1)))
         ((== x 2)
          ;; generated code: should get these two goals as elements of the conde branch
          (later (== 1 1))
          (later (== q 'branch-2)))
         ((== x 2) (later (== q 'branch-3))))))))
 '(branch-2 branch-3))
(generated-code)

(test
 (run* (q)
   (staged
    (fresh (x)
      (== x 2)
      (gather
       (conde
         ((== x 1) (later (== q 'branch-1)))
         ((== x 2)
          ;; generated code: should get a conj of these but no conde
          (later (== 1 1))
          (later (== q 'branch-2))))))))
 '(branch-2))
(generated-code)

(test
 (run* (q)
   (staged
    (fresh (x)
      (== x 3)
      (conde
        ((gather
          (conde
            ((== x 1) (later (== q 'branch-1)))
            ((== x 2) (later (== q 'branch-2))))))
        ((== x 3) (later (== q 'branch-3)))))))
 '(branch-3))

(test
 (length
  (run* (q1 q2)
    (staged
     (fresh (x y)
       (gather
        (conde
          ((== x 1) (later (== q1 'branch-1)))
          ((== x 2) (later (== q1 'branch-2)))))
       (gather
        (conde
          ((== y 1) (later (== q2 'branch-1)))
          ((== y 2) (later (== q2 'branch-2)))))))))
 4)

(defrel/generator (nevero)
  (conde
    [fail]
    [(nevero)]))

(test
 (run* (q)
   (staged
    (fallback
     (later (== q 'fallback))
     (conde
       ((== q 'branch-1))
       ((gather
         (conde
           ((later (== q 'branch-2)))
           ((nevero)))))))))
 '(fallback))