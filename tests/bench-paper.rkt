#lang racket

;; Staging simple relations (vs unstaged)
(require "applications/replicate.rkt")
(require "applications/power.rkt")


;; Staging the evalo relation: write append as a function, compile as relation (thus faster flexible functions)
;;
(require "synth-task-macros-backwards.rkt")
;; (require "interpreter/basics.rkt")
;; --(require "../small-interp/staged.rkt")
;; -- (require "../small-interp/unstaged.rkt")
;;
;; sub-block: eval-like relations on program-like arguments
(require "applications/grammars.rkt")

;; Staging the evalo relation running on (eval <program-text>) (thus faster eval functions in particular)
(require "applications/or-lang-interp.rkt")
(require (submod "applications/proof.rkt" benchmark))
(require "applications/peano-fib.rkt")
(require "applications/parsing-with-derivatives.rkt")
(require "applications/double-eval.rkt") ;; quines w/quasiquotes
(require "applications/dl.rkt") ;; Negation-normal form

;; Synthesis w/ground context
;; When we rely on a built-in primitive implemented in the object 'eval', you get a neat multi-level compiled-interpreted interaction
(require "applications/metaKanren.rkt")
(require "applications/map.rkt") ;; the example we wrote just for the purpose
(require "synth-task-macros-synth-context.rkt") ;; synthesize part of append from context
