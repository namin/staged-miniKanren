#lang racket/base

(require "staged-dsl.rkt")
(require "staged-interp-tiny.rkt")

(test
    (run-staged 1 (q)
      (tiny-eval-expo 1 '() q))
  '(1)
  )

(test
    (run 1 (q)
      (u-tiny-eval-expo 1 '() q))
  '(1)
  )

(test
    (run-staged 1 (q)
      (tiny-eval-expo ''(hello world) '() q))
  '((hello world))
  )

(test
    (run 1 (q)
      (u-tiny-eval-expo ''(hello world) '() q))
  '((hello world))
  )
