#lang racket/base

(require "../main.rkt"
         "../test-check.rkt")

(defrel (eval-ambo e v)
  (conde
    ((numbero e)
     (== e v))
    ((fresh (e1 e2 v1 v2)
       (== e `(cons ,e1 ,e2))
       (== v (cons v1 v2))
       (eval-ambo e1 v1)
       (eval-ambo e2 v2)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (conde
         ((eval-ambo e1 v))
         ((eval-ambo e2 v)))))))

(test
  (run* (v) (eval-ambo '(amb 1 2) v))
  '(1 2))

(defrel/generator (gen-eval-ambo e v)
  (conde
    ((numbero e)
     (later (== e v)))
    ((fresh (e1 e2 v1 v2)
       (== e `(cons ,e1 ,e2))
       (later (== v (cons v1 v2)))
       (gen-eval-ambo e1 v1)
       (gen-eval-ambo e2 v2)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (gather
        (conde
          ((gen-eval-ambo e1 v))
          ((gen-eval-ambo e2 v))))))))

(test
  (run* (v) (staged (gen-eval-ambo '(amb 1 2) v)))
  '(1 2))

;; non-deterministic
;; (run 2 (e v) (staged (gen-eval-ambo `(cons (amb 1 2) ,e) v)))

(defrel/multistage/fallback (ms-eval-ambo e v)
  (conde
    ((numbero e)
     (later (== e v)))
    ((fresh (e1 e2 v1 v2)
       (== e `(cons ,e1 ,e2))
       (later (== v (cons v1 v2)))
       (ms-eval-ambo e1 v1)
       (ms-eval-ambo e2 v2)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (gather
        (conde
          ((ms-eval-ambo e1 v))
          ((ms-eval-ambo e2 v))))))))

(test
  (run* (v) (staged (ms-eval-ambo '(amb 1 2) v)))
  '(1 2))

(run 4 (e v) (staged (ms-eval-ambo `(cons (amb 1 2) ,e) v)))

(defrel (lookupo x env v)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
     [(== x y) (== v b)]
     [(=/= x y) (lookupo x rest v)])))

(defrel-partial (apply-lambda-ambo rep [x e env] [arg v])
  (eval-lambda-ambo e (cons (cons x arg) env) v))

(defrel (eval-lambda-ambo e env v)
  (conde
    ((numbero e)
     (== e v))
    ((fresh (e1 e2 v1 v2)
       (== e `(cons ,e1 ,e2))
       (== v (cons v1 v2))
       (eval-lambda-ambo e1 env v1)
       (eval-lambda-ambo e2 env v2)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (conde
         ((eval-lambda-ambo e1 env v))
         ((eval-lambda-ambo e2 env v)))))
    ((symbolo e)
     (lookupo e env v))
    ((fresh (x e0)
       (== e `(lambda (,x) ,e0))
       (== v (partial-apply apply-lambda-ambo x e0 env))))
    ((fresh (e1 e2 v1 v2)
       (== e `(,e1 ,e2))
       (eval-lambda-ambo e1 env v1)
       (eval-lambda-ambo e2 env v2)
       (finish-apply v1 apply-lambda-ambo v2 v)))))

(test
  (run* (v) (eval-lambda-ambo '(amb 1 2) '() v))
  '(1 2))

(test
  (run* (v) (eval-lambda-ambo '((lambda (x) (amb x 3)) (amb 1 2)) '() v))
  '(3 3 1 2))
