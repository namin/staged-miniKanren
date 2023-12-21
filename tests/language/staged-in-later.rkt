#lang racket/base

(require "../../main.rkt"
         "../../test-check.rkt")

(test
 (run 1 (q)
   (staged
    (later
     (fresh (x y)
       (== q (list x y))
       (staged
        (conde
          [fail]
          [(later
            (== x 1))]))))))
 '((1 _.0)))