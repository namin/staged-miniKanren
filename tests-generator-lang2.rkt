#lang racket/base

(require "generator-lang2.rkt"
         "test-check.rkt")

(defrel (foo a)
  (== a 5))

(test
 (run 1 (q) (foo 5))
 '(_.0))

(test
 (run 1 (q)
   (staged
    (later
     (== q 1))))
 '(1))

(defrel (bar a)
  (staged
   (later
    (== a 1))))

(test
 (run 1 (q)
   (bar q))
 '(1))