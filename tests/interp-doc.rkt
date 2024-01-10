#lang racket/base

(require racket/pretty
         "../main.rkt"
         "../test-check.rkt")

#;(if-null? xs a b)
#;(match xs ['() a] [(cons x xs) b])

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
     ((numbero e)
      (later (== e v)))
     ((fresh (e1 e2 v1 v2)
        (== e `(cons ,e1 ,e2))
        (later (== v (cons v1 v2)))
        (evalo e1 env v1)
        (evalo e2 env v2)))
     ((fresh (e1 e2)
        (== e `(amb ,e1 ,e2))
        (gather
         (conde
           ((evalo e1 env v))
           ((evalo e2 env v))))))
     ((fresh (env-v)
        (symbolo e)
        ;; We want to make the unifications with v later-stage.
        ;; We put it before the lookupo to make sure the runtime version knows the value.
        (later (== v env-v))
        (lookupo e env env-v)))
     ((fresh (x e0)
        (== e `(lambda (,x) ,e0))
        (specialize-partial-apply v applyo x e0 env)))
     ((fresh (e1 e2 v1 v2)
        (== e `(,e1 ,e2))
        (evalo e1 env v1)
        (evalo e2 env v2)
        (later (finish-apply v1 applyo v2 v)))))))

(test
  (run* (v) (staged (evalo '(amb 1 2) '()  v)))
  '(1 2))

(run 4 (e v) (staged (evalo `(cons (amb 1 2) ,e) '()  v)))

(test
  (run* (v) (staged (evalo '((lambda (x) (amb x 3)) (amb 1 2)) '() v)))
  '(3 3 1 2))
