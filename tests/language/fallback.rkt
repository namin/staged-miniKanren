#lang racket/base

(require "../../main.rkt"
         "../../test-check.rkt")

(test
 (run 1 (q)
      (staged
       (fallback
        (later (== 1 q))
        (conde
          ((== q 2) (later (== q 2)))))))
 '(2))

(test
 (run 1 (q)
      (staged
       (fallback
        (later (== 1 q))
        (conde
          ((== 2 q) (later (== 2 q)))
          ((== 3 q) (later (== 3 q)))))))
 '(1))

(test
 (run 1 (q)
      (staged
       (fresh ()
         (== q 3)
         (fallback
          (later (== 1 q))
          (conde
            ((== 2 q) (later (== 2 q)))
            ((== 3 q) (later (== 3 q))))))))
 '(3))

(test
 (run 1 (q)
      (staged
       (fallback
        (later (== 1 q))
        (conde
          ((== 2 q) (later (== q 1)))))))
 '())

(test
 (run 1 (q)
      (staged
       (fallback
        (later (== 1 q))
        (conde
          ((fresh (x) (== 2 x) (later (== q x))))))))
 '(2))
