#lang racket/base

(require "staged-dsl.rkt")
(provide tiny-eval-expo u-tiny-eval-expo)

(define (tiny-eval-expo expr env val)
  (condg
    (later `(u-tiny-eval-expo ,(expand expr) ,(expand env) ,(expand val)))
    ([] [(numbero expr)] [(l== expr val)])
    ([v]
     [(== `(quote ,v) expr)]
     [(l== val v)])
    ))

(define (u-tiny-eval-expo expr env val)
  (conde
    ((numbero expr) (== expr val))
    ((fresh (v)
       (== `(quote ,v) expr)
       (== val v)))))
