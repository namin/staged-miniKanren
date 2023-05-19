#lang racket/base

(require "../../all.rkt")

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
 (run* (q)
   (staged
    (fresh (x)
      (== x 2)
      (gather
       (conde
         ((== x 1) (later (== q 'branch-1)))
         ((== x 2) (later (== q 'branch-2))))))))
 '(branch-2))

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