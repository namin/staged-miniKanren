#lang racket/base

(require "internals.rkt")

(provide

 define-relation
 run run*
 == =/=
 fresh
 ;; TODO: generator code shouldn't use conde, but unstaged fallbacks are currently
 ;; written in the same language because of the mutual dependency between staged and unstaged.
 conde
 succeed
 fail
 symbolo numbero stringo
 absento
 project
 
 run-staged
 run-staged*
 define-staged-relation

 condg
 
 lapp
 lconde

 lsucceed
 lfail
 l==
 l=/=
 lsymbolo
 lnumbero
 lstringo
 labsento

 partial-apply
 lpartial-apply
 apply-partial
 lapply-partial
 
 generated-code
 reset-generated-code!
)