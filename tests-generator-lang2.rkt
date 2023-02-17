#lang racket/base

(require "generator-lang2.rkt")

(defrel (foo a)
  (== a 5))

(run 1 (q) (foo q))