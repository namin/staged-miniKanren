#lang racket/base

(require "staged-load.rkt")

(define (rel arg)
  (l== arg 5))

(run-staged 1 (q) (rel q))
