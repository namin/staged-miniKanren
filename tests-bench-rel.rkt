#lang racket/load

(require "generator-lang2.rkt"
         "staged-interp.rkt"
         "test-check.rkt"
         "staged-test-tags.rkt")

(load "tests-parsing-with-derivatives.scm")
(load "tests-proof.scm")
(load "tests-dl.scm")
(load "tests-peano-fib.scm")
