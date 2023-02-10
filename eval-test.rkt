#lang racket/base

(require "staged-load.rkt")

(define (rel arg)
  (== arg 5))

(run-staged 1 (q) (lapp rel q))

