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
     ((numbero e) (== e v))
     ((fresh (e1 e2 v1 v2)
        (== e `(cons ,e1 ,e2))
        (== v (cons v1 v2))
        (evalo e1 env v1)
        (evalo e2 env v2)))
     ((fresh (ve res null-e pair-e first-x rest-x)
        (== e `(match ,ve ['() ,null-e] [(cons ,first-x ,rest-x) ,pair-e]))
        (symbolo first-x)
        (symbolo rest-x)
        (evalo ve env res)
        (gather
         (conde
           ((== res '()) (evalo null-e env v))
           ((fresh (first rest)
              (== res (cons first rest))
              (evalo pair-e
                     `((,first-x . ,first) (,rest-x . ,rest) . ,env)
                     v)))))))
     ((symbolo e)
      (lookupo e env v))
     ((fresh (x e0)
        (== e `(lambda (,x) ,e0))
        (specialize-partial-apply v applyo x e0 env)))
     ((fresh (e1 e2 v1 v2)
        (== e `(,e1 ,e2))
        (evalo e1 env v1)
        (evalo e2 env v2)
        (later (finish-apply v1 applyo v2 v)))))))

(test
 (run 4 (e v) (staged (evalo `(cons 1 ,e) '()  v)))
 '(((_.0 (1 . _.0)) $$ (num _.0))
  (((cons _.0 _.1) (1 _.0 . _.1)) $$ (num _.0 _.1))
  ((lambda (_.0) _.1) (1 . #s(apply-rep applyo (_.0 _.1 ()) #f)))
  (((cons _.0 (cons _.1 _.2)) (1 _.0 _.1 . _.2)) $$ (num _.0 _.1 _.2))))

(test
 (run 1 (q) (staged (evalo `(match (cons 1 2) ['() 0] [(cons a d) d]) '() q)))
 '(2))

(generated-code)

