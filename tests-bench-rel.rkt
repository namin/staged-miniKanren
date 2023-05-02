#lang racket/base

(require "generator-lang2.rkt"
         "staged-interp.rkt"
         "test-check.rkt"
         "staged-test-tags.rkt"
         racket/include)

(include "tests-parsing-with-derivatives.scm")
(include "tests-proof.scm")
(include "tests-dl.scm")
(include "tests-peano-fib.scm")
