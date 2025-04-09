#lang racket

(require staged-miniKanren)
(require "../test-check.rkt")

(defrel (booleano v)
  (conde [(== v #t)] [(== v #f)]))

(defrel (lookupo x env v)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      [(== x y) (== v b)]
      [(=/= x y) (lookupo x rest v)])))

(defrel (make-closo x e env clos)
  (== clos `(clos ,x ,e ,env)))

(defrel (apply-closo clos v1 v)
  (fresh (x e env)
    (== clos `(clos ,x ,e ,env))
    (evalo-or e `((,x . ,v1) . ,env) v)))

(defrel (evalo-or e env v)
  (conde
    [(booleano e) (== e v)]
    [(fresh (e1 e2 v1)
       (== e `(or ,e1 ,e2))
       (evalo-or e1 env v1)
       (conde
         [(== v1 #f) (evalo-or e2 env v)]
         [(=/= v1 #f) (== v1 v)]))]
    [(symbolo e) (lookupo e env v)]
    [(fresh (x e1)
       (== e `(lambda (,x) ,e1))
       (symbolo x)
       (make-closo x e1 env v))]
    [(fresh (e1 e2 v1 v2)
       (== e `(,e1 ,e2))
       (evalo-or e1 env v1)
       (evalo-or e2 env v2)
       (apply-closo v1 v2 v))]))

(test
 (run 20 (q)
   (evalo-or q '() #t))
 '(#t
   (or #t _.0)
   (or #f #t)
   (or (or #t _.0) _.1)
   (or #f (or #t _.0))
   (((lambda (_.0) #t) #t) $$ (sym _.0))
   (or (or #f #t) _.0)
   (or #f (or #f #t))
   (((lambda (_.0) _.0) #t) $$ (sym _.0))
   (((lambda (_.0) (or #t _.1)) #t) $$ (sym _.0))
   (or (or #f #f) #t)
   (((lambda (_.0) #t) #f) $$ (sym _.0))
   (((lambda (_.0) (or #f #t)) #t) $$ (sym _.0))
   (or #f (or (or #t _.0) _.1))
   (or (or #f #f) (or #t _.0))
   (((lambda (_.0) (or #t _.1)) #f) $$ (sym _.0))
   (((lambda (_.0) (or _.0 _.1)) #t) $$ (sym _.0))
   (or #f (or #f (or #t _.0)))
   (((lambda (_.0) (or #f _.0)) #t) $$ (sym _.0))
   (((lambda (_.0) (or (or #t _.1) _.2)) #t) $$ (sym _.0))))

(test
 (run 1 (p q)
   (evalo-or `((,p (x) x) #t) '() q))
 '((lambda #t)))
