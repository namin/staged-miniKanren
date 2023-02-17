#lang racket/base

(require "staged-load.rkt")

(provide

 define-relation
 run run*
 == =/=
 fresh
 ;; TODO: generator code shouldn't use conde, but unstaged fallbacks are currently
 ;; written in the same language because of the mutual dependency between staged and unstaged.
 conde
 symbolo numbero stringo
 absento
 
 run-staged
 run-staged*
 define-staged-relation

 condg
 
 lapp
 lconde
 
 lfail
 l==
 l=/=
 lsymbolo
 lnumbero
 lstringo
 labsento

 reify-call
 lreify-call
 apply-reified
 lapply-reified
 
 generated-code
 reset-generated-code!
)