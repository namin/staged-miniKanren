#lang racket/base

(require "../../all.rkt")

(defrel/staged/fallback (!-arith e t)
  (conde
    [(== t 'int) (later (numbero e))]
    [(== t 'bool)
     (gather
      (conde
        [(later (== #f e))]
        [(later (== #t e))]))]))

(run 4 (e t)
  (staged
   (!-arith e t)))

(run 4 (e)
  (staged
   (!-arith e 'int)))
(generated-code)

(run 4 (e)
  (staged
   (!-arith e 'bool)))
(generated-code)

(defrel/staged (lookupo x env t)
  (fresh (rest y v)
    (== `((,y . ,v) . ,rest) env)
    (conde
      ((== y x) (== v t))
      ((=/= y x) (lookupo x rest t)))))

(defrel/staged/fallback (!-arith2 e env t)
  (conde
    [(== t 'int)
     (gather
      (conde
        [(later (numbero e))]
        [(lookupo e env t)]))]
    [(== t 'bool)
     (gather
      (conde
        [(later (== #f e))]
        [(later (== #t e))]
        [(lookupo e env t)]))]))

(run 4 (e)
  (staged
   (!-arith2 e '() 'int)))

(run 4 (e)
  (staged
   (!-arith2 e '((x . int)) 'int)))
(generated-code)

(run 4 (e)
  (staged
   (!-arith2 e '((x . int) (y . int)) 'int)))
(generated-code)


;; t is int
;; env is ((x . int) (y . int) (z . bool))
(defrel (!-arith-manual e)
  (conde
    [(numbero e)]
    [(== 'x e)]
    [(== 'y e)]
    [(fresh (e1 e2)
       (== `(+ ,e1 ,e2) e)
       (!-arith-manual e1)
       (!-arith-manual e2))]))

(defrel/staged/fallback (!-arith-int e env)
  (gather
   (conde
     [(numbero e)]
     [(lookupo e env 'int)]
     [(fresh (e1 e2)
        (== `(+ ,e1 ,e2) e)
        (later (!-arith-int e1 env))
        (later (!-arith-int e2 env)))])))

;; assuming we want the type (list int)
;; then one value is car(list(list(5)))
;; but this is not in normal form!

;; assuming we want the type (-> int (list int))
;; (lambda (x) ...)

;; 1. values in normal form
;; 2. simulate something like tabling

;; can we get away with passing an explicit memo table?

(defrel/staged/fallback (!-arith3 e env t)
  (conde
    [(== t 'int)
     (gather
      (conde
        [(later (numbero e))]
        [(lookupo e env t)]
        [(fresh (e1 e2)
           (later (== `(+ ,e1 ,e2) e))
           (later (!-arith3 e1 env 'int))
           (later (!-arith3 e2 env 'int)))]))]
    [(== t 'bool)
     (gather
      (conde
        [(later (== #f e))]
        [(later (== #t e))]
        [(lookupo e env t)]))]))

(run 10 (e)
  (staged
   (!-arith3 e '((x . int) (y . int)) 'int)))
(generated-code)
