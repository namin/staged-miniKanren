#lang racket/base

(require "generator-lang2.rkt"
         "staged-interp.rkt"
         "test-check.rkt")

(test
 (run 1 (q)
      (staged
       (evalo-staged
        `(letrec ((f (lambda (x) x))) 1)
        q)))
 '(1))

(test
 (run 1 (q)
      (staged
       (evalo-staged
        `(letrec ((f (lambda (x) x))) (f 1))
        q)))
 '(1))

(test
 (run 1 (q)
      (staged
       (evalo-staged
        `(letrec ((f (lambda x (car x)))) (f 1))
        q)))
 '(1))

(test
 (run 1 (q)
      (staged
       (evalo-staged
        `(letrec ((f (lambda (x) (if (pair? x) (f (car x)) x)))) (f '(1 2)))
        q)))
 '(1))

(test
 (run 1 (q)
      (staged
       (evalo-staged
        `(letrec ((f (lambda x (if (pair? (car x)) (f (car (car x))) (car x))))) (f '(1 2)))
        q)))
 '(1))
