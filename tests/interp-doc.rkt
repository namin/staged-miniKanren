#lang racket/base

(require racket/pretty
         "../main.rkt"
         "../test-check.rkt")

(defrel/staged (booleano v)
  (conde [(== v #t)] [(== v #f)]))

(defrel/staged (lookupo x env v)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (fallback
     (conde
       [(== x y) (== v b)]
       [(=/= x y) (lookupo x rest v)]))))

(defrel-partial/staged (applyo rep [x e env] [arg v])
  (evalo e (cons (cons x arg) env) v))

(defrel/staged (evalo e env v)
  (fallback
   (conde
     ((booleano e) (== e v))
     ((fresh (e1 e2 v1)
        (== e `(or ,e1 ,e2))
        (evalo e1 env v1)
        (gather
         (conde
           [(== v1 #f) (evalo e2 env v)]
           [(=/= v1 #f) (== v1 v)]))))
     ((symbolo e) (lookupo e env v))
     ((fresh (x e0)
        (== e `(lambda (,x) ,e0))
        (specialize-partial-apply v applyo x e0 env)))
     ((fresh (e1 e2 v1 v2)
        (== e `(,e1 ,e2))
        (evalo e1 env v1)
        (evalo e2 env v2)
        (later (finish-apply v1 applyo v2 v)))))))

(run 3 (e v) (staged (evalo `(or (or #f ,e) #f) '() v)))

(run 3 (e v) (staged (evalo `((lambda (x) (or (or #f ,e) x)) #f) '() v)))

(run 3 (e v) (staged (evalo `((lambda (x) x) #f) '()  v)))

(run 3 (e v) (staged (evalo `((lambda (x) (or #f x)) #f) '()  v)))

(run 3 (e v) (staged (evalo `((lambda (x) (or ,e x)) #f) '()  v)))

(run 3 (e v) (staged (evalo `((lambda (f) (or (f #t) (f #f))) (lambda (x) x)) '() v)))
(generated-code)

