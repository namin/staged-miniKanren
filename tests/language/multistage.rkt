#lang racket/base

(require "../../main.rkt"
         "../../test-check.rkt")

(defrel/multistage/explicit (unify-mystery x)
  #:runtime
  (== x 5)
  #:staging-time
  (later (== x 6)))

(test
  (run* (q)
    (unify-mystery q))
  '(5))

(test
  (run* (q)
    (staged
     (unify-mystery q)))
  '(6))
