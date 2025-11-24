#lang racket/base

(require "../all.rkt")
(require "pldi/appendo.rkt")

;; functions vs relations
;; append vs appendo
;; run in multiple directions
;; run with partially instantiated terms
;; so far, similar in miniKanren than Prolog

;; relational interpreter
;; turning functions into relations
;; ex: pldi/appendo.rkt
;; if function is itself an interpreter, get a synthesizer
;; ex: pldi/regex.rkt

;; functional synthesis with the relational interpreter
;; ex: pldi/synth-append.rkt

;; staging: a staged interpreter becomes a compiler
;; we stage the relational interpreter
;; we get a compiler from functions to relations
;; ex: pldi/proofo.rkt
;; we get faster functional synthesis
;; ex: pldi/synth-fib.rkt
;; ex: pldi/quasiquote.rkt

;; under the hood
;; how do we define relations: from append to appendo
;; ex: pldi/appendo.rkt
;; how do we define relational interpreters:
;; ex: paper side-by-side example from ICFP'17
;; ex: pldi/or-evalo.rkt



