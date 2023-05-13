#lang racket/base

(require "../../all.rkt")

#;(run 1 (q)
     (staged
      (fresh (x)
        (conde
         [(== x 1) (later (== q 1))]
         [(== x 2) (later (== q 2))])
        (== x 1))))

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
     (fresh (a b)
       (condg
        #:fallback (later (rt a b))
        ([]
         [(== a 5)]
         [(later (== b 1))])
        ([]
         [(== a 6)]
         [(later (== b 2))])))])))

#;(run 1 (q) (fresh (y) (staged (st 1 q))))

#;(generated-code)

#;(run 1 (q) (fresh (y) (staged (fresh ()
                                (st y q)
                                (== y 5)))))

#;(generated-code)

(run 1 (q) (fresh (y) (staged (st 2 q)))) ;; should commit re: outer, fallback re: inner
  
(generated-code)

#;(run 1 (q) (staged (evalo-staged '(lambda (x) 5) q)))

#;(generated-code)

(defrel/generator (r-g a b)
  (later (== a b)))

(defrel-partial (r [a] [b])
  #:generator r-g
  (== a b))

(run 1 (q)
     (staged (condg #:fallback (later (== q 3))
                    [[] [] [(later (== q (partial-apply r 5)))]]
                    [[] [] [(later (== q 1))]])))
