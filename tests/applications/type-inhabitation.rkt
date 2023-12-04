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
(generated-code)

(run 4 (e)
  (staged
   (!-arith e 'int)))
(generated-code)

(run 4 (e)
  (staged
   (!-arith e 'bool)))
(generated-code)

(defrel/staged (lookupof x env t)
  (conde
    [(== env '()) (== t #f)]
    [(fresh (rest y v)
       (== `((,y . ,v) . ,rest) env)
       (=/= v #f)
       (conde
         ((== y x) (== v t))
         ((=/= y x) (lookupof x rest t))))]))

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

(run 10 (e)
  (staged
   (!-arith-int e '((x . int) (y . int)))))
(generated-code)

;; assuming we want the type (list int)
;; then one value is car(list(list(5)))
;; but this is not in normal form!

;; assuming we want the type (-> int (list int))
;; (lambda (x) ...)

;; 1. values in normal form
;; 2. simulate something like tabling

;; can we get away with passing an explicit memo table?

(defrel-partial/staged (!-arith-memo rep [t env memo] [e])
  (fresh (memo^)
    (trace !- rep memo)
    (== (cons (cons t rep) memo) memo^)
    (conde
      [(== t 'int)
       (gather
        (conde
          [(later (numbero e))]
          [(lookupo e env t)]
          [(fresh (e1 e2)
             (== `(+ ,e1 ,e2) e)
             (!-arith-memo-wrapper 'int env memo^ e1)
             (!-arith-memo-wrapper 'int env memo^ e2))]))]
      [(== t 'bool)
       (gather
        (conde
          [(later (== #f e))]
          [(later (== #t e))]
          [(lookupo e env t)]))])))

(defrel/staged (!-arith-memo-wrapper t env memo e)
  (fresh (rep rep^)
    (lookupof t memo rep) ;; TODO: need env if env grows!
    (conde
      [(== rep #f)
       (specialize-partial-apply rep^ !-arith-memo t env memo)]
      [(=/= rep #f)
       (== rep rep^)])
    (later (finish-apply rep^ !-arith-memo e))))

(run 10 (e)
  (staged
   (!-arith-memo-wrapper 'int '() '() e)))
(generated-code)

(defrel (make-diff q)
  (fresh (x)
    (== q (cons x x))))

(defrel (add-diff q1 el q2)
  (fresh (x l y)
    (== (cons x l) q1)
    (== x (cons el y))
    (== (cons y l) q2)))

(run* (q1 q2 q3)
  (make-diff q1)
  (add-diff q1 'hello q2)
  (add-diff q2 'world q3))

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
