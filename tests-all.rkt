#lang racket/base

(require "tests-run.rkt")
(require "tests-generator-lang2.rkt")
(require "tests-doc.rkt")
(require "tests-staged-apply-letrec.rkt")

;; these are looking at innards using staged-load.rkt
(require "tests-staged-apply.rkt")
(require "tests-fix-scope.rkt")
(require "tests-condg.rkt")