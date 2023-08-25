#lang racket/base

(require racket/pretty
         "../main.rkt"
         "../test-check.rkt")

(defrel/multistage/fallback (!- exp env t)
  (conde
    [(fresh (env-t)
       (symbolo exp)
       (later (== t env-t))
       (lookupo exp env env-t))]
    [(fresh (x e t-x t-e)
       (== `(lambda (,x) ,e) exp)
       (symbolo x)
       (not-in-envo 'lambda env)
       (later (== `(-> ,t-x ,t-e) t))
       (!- e `((,x . ,t-x) . ,env) t-e))]
    [(fresh (rator rand t-x)
       (== `(,rator ,rand) exp)
       (!- rator env `(-> ,t-x ,t))
       (!- rand env t-x))]))

(defrel/multistage/fallback (lookupo x env t)
  (fresh (rest y v)
    (== `((,y . ,v) . ,rest) env)
    (conde
      ((== y x) (== v t))
      ((=/= y x) (lookupo x rest t)))))

(defrel/multistage/fallback (not-in-envo x env)
  (conde
    ((== '() env))
    ((fresh (y v rest)
       (== `((,y . ,v) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))

(test
  (run 1 (t) (staged (!- '(lambda (x) (lambda (f) (f x))) '() t)))
  '((-> _.0 (-> (-> _.0 _.1) _.1))))

(test
  (run 3 (e t) (staged (!- `(lambda (x) (lambda (f) (f ,e))) '() t)))
  '((x (-> _.0 (-> (-> _.0 _.1) _.1)))
    (((lambda (_.0) _.0) (-> _.1 (-> (-> (-> _.2 _.2) _.3) _.3))) $$ (sym _.0))
    (((lambda (_.0) x) (-> _.1 (-> (-> (-> _.2 _.1) _.3) _.3)))
     $$
     (=/= ((_.0 x)))
     (sym _.0))))

(test
  (run 1 (t) (staged (!- '(lambda (x) (x x)) '() t)))
  '())

(pretty-print
 (generated-code))

(test
 (run 1 (t)
   (staged (!- '((app f) x)
               '((app . (-> (-> a b) (-> a b)))
                 (f . (-> a b))
                 (x . a))
               t)))
 '(b))

(test
  (run 3 (e t)
    (fresh (ef ex)
      (== e `((app ,ef) ,ex))
      (staged (!- e
                  '((app . (-> (-> a b) (-> a b)))
                    (f . (-> a b))
                    (x . a))
                  t))))
  '((((app f) x) b)
    (((app (app f)) x) b)
    ((((app f) ((lambda (_.0) _.0) x)) b) $$ (sym _.0))))

(pretty-print
 (generated-code))
