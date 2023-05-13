#lang racket/base

(require "../../all.rkt")

(defrel (rt x y)
  (conde
   [(== x 1) (== y 1)]
   [(== x 2) (== y 2)]))

(defrel/generator (st x y)
  (condg
   #:fallback (later (rt x y))
   ([]
    [(== x 1)]
    [(later (== y 1))])
   ([]
    [(== x 2)]
    [(later (== y 2))
     (fresh (x y)
       (condg
        #:fallback (later (rt x y))
        ([]
         [(== x 1)]
         [(later (== y 1))])
        ([]
         [(== x 2)]
         [(later (== y 2))])))])))

(run 1 (q) (fresh (y) (staged (st 2 q)))) ;; should commit re: outer, fallback re: inner
  
(generated-code)

#;(run 1 (q) (staged (evalo-staged '(lambda (x) 5) q)))

#;(generated-code)