#lang racket/base

(provide (all-defined-out))

(require racket/list
         racket/include
         racket/match
         racket/set
         syntax/parse
         (for-syntax racket/base syntax/parse))

(include "../faster-minikanren/racket-compatibility.scm")
(include "../faster-minikanren/mk.scm")
(include "../faster-miniKanren/staged-mk.scm")
(include "staged-apply.scm")
(include "condg.rktl")
(include "staged-utils.scm")
(include "staged-run.scm")
(include "test-check.scm")

(define (mapo fo xs ys)
  (conde
    ((== xs '()) (== ys '()))
    ((fresh (xa xd ya yd)
       (== xs (cons xa xd))
       (== ys (cons ya yd))
       (fo xa ya)
       (mapo fo xd yd)))))

(define (make-list-of-symso xs ys)
  (mapo (lambda (x y) (== y (unexpand x))) xs ys))

(define (varo x)
  (lambda (c)
    (if (var? (walk* x (state-S c)))
        c
        #f)))

(define (non-varo x)
  (lambda (c)
    (if (var? (walk* x (state-S c)))
        #f
        c)))

(define (logo f . args)
  (lambda (c)
    (apply printf f (walk* args (state-S c)))
    (newline)
    c))
