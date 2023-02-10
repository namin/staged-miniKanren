#lang racket/base

(require "generator-lang.rkt"
         racket/include
         (for-syntax racket/base syntax/parse))

(include "test-check.scm")

(provide record-bench
         test
         time-test
         todo)