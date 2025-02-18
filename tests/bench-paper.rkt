#lang racket

;; Staging simple relations (vs unstaged)
(require "applications/jbh_replicate.rkt") ;; Done + category
(require "applications/jbh_power.rkt")     ;; Done + category


;; Staging the evalo relation: write append as a function, compile as relation (thus faster flexible functions)
;;
(require "jbh_synth-task-macros-backwards.rkt") ;; just the running backwards ;; Done + category
(require "interpreter/jbh_basics.rkt") ;; get append examples ;; Done + category
;; --(require "../small-interp/staged.rkt")
;; -- (require "../small-interp/unstaged.rkt")
;; SKIP
;; (require "applications/dl.rkt") ;; Negation-normal form
;;
;; sub-block: eval-like relations on program-like arguments
(require "applications/jbh_grammars.rkt") ;; Done + category

;; Staging the evalo relation running on (eval <program-text>) (thus faster eval functions in particular)
(require "applications/jbh_or-lang-interp.rkt") ;; Done + category
(require "applications/jbh_proof.rkt") ;; Done + category
(require "applications/jbh_parsing-with-derivatives.rkt") ;; Done, already there + category
(require "applications/jbh_double-eval.rkt") ;; quines w/quasiquotes ;; Done, already there. + category

;; Synthesis w/ground context
;; When we rely on a built-in primitive implemented in the object 'eval', you get a neat multi-level compiled-interpreted interaction
(require "applications/jbh_map.rkt") ;; the example we wrote just for the purpose Done + category
(require "jbh_synth-task-macros-synth-context.rkt") ;; synthesize part of append from context ;; Done + category
