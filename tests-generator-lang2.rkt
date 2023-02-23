#lang racket/base

(require "generator-lang2.rkt"
         "test-check.rkt")

(defrel (foo a)
  (== a 5))

(test
 (run 1 (q) (foo 5))
 '(_.0))

(defrel (foo a)
  (== a 5))