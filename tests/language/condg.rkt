#lang racket/base

(require "../../main.rkt"
         "../../test-check.rkt")

(test
    (run 1 (q) (condg #:fallback (== 1 q) (() [(== 2 q)] [(== 1 1)])))
  '(2))

(test
    (run 1 (q) (condg #:fallback (== 1 q)
                      (() [(== 2 q)] [(== 1 1)])
                      (() [(== 3 q)] [(== 1 1)])))
  '(1))

(test
    (run 1 (q)
      (== q 3)
      (condg #:fallback (== 1 q)
             (() [(== 2 q)] [(== 1 1)])
             (() [(== 3 q)] [(== 1 1)])))
  '(3))

(test
    (run 1 (q) (condg #:fallback (== 1 q) (() [(== 2 q)] [(== q 1)])))
  '())

(test
    (run 1 (q) (condg #:fallback (== 1 q) ((x) [(== 2 x)] [(== q x)])))
  '(2))
