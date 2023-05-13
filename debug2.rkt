#lang racket/base

(require "all.rkt")

(run 1 (q x)
      (staged
       (fresh ()
         (symbolo x)
         (evalo-staged `((lambda (,x) x) 1) q))))

(generated-code)