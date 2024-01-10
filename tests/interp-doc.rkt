#lang racket/base

(require racket/pretty
         "../main.rkt"
         "../test-check.rkt")

(if-null? xs a b)
(match xs ['() a] [(cons x xs) b])

(defrel/staged (ms-lookupo x env v)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (fallback
     (conde
       [(== x y) (== v b)]
       [(=/= x y) (ms-lookupo x rest v)]))))

(defrel-partial/staged (ms-apply-lambda-ambo rep [x e env] [arg v])
  (ms-eval-lambda-ambo e (cons (cons x arg) env) v))

(defrel/staged (ms-eval-lambda-ambo e env v)
  (fallback
   (conde
     ((numbero e)
      (later (== e v)))
     ((fresh (e1 e2 v1 v2)
        (== e `(cons ,e1 ,e2))
        (later (== v (cons v1 v2)))
        (ms-eval-lambda-ambo e1 env v1)
        (ms-eval-lambda-ambo e2 env v2)))
     ((fresh (e1 e2)
        (== e `(amb ,e1 ,e2))
        (gather
         (conde
           ((ms-eval-lambda-ambo e1 env v))
           ((ms-eval-lambda-ambo e2 env v))))))
     ((fresh (env-v)
        (symbolo e)
        ;; We want to make the unifications with v later-stage.
        ;; We put it before the lookupo to make sure the runtime version knows the value.
        (later (== v env-v))
        (ms-lookupo e env env-v)))
     ((fresh (x e0)
        (== e `(lambda (,x) ,e0))
        (specialize-partial-apply v ms-apply-lambda-ambo x e0 env)))
     ((fresh (e1 e2 v1 v2)
        (== e `(,e1 ,e2))
        (ms-eval-lambda-ambo e1 env v1)
        (ms-eval-lambda-ambo e2 env v2)
        (later (finish-apply v1 ms-apply-lambda-ambo v2 v)))))))

(test
  (run* (v) (staged (ms-eval-lambda-ambo '(amb 1 2) '()  v)))
  '(1 2))

(run 4 (e v) (staged (ms-eval-lambda-ambo `(cons (amb 1 2) ,e) '()  v)))

(test
  (run* (v) (staged (ms-eval-lambda-ambo '((lambda (x) (amb x 3)) (amb 1 2)) '() v)))
  '(3 3 1 2))
