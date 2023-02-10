#lang racket/base

(provide
 evalo-staged
 eval-expo

 evalo-unstaged
 u-eval-expo)

(require "generator-lang.rkt"
         
         ;; things that really shouldn't be used in enerator code, but we haven't finished
         ;; transforming the interpreter to use condg yet
         (only-in "staged-load.rkt"
                  varo
                  non-varo)
         
         racket/include)

(include "unstaged-interp.scm")
(include "staged-interp.scm")