#lang racket

(require "../../all.rkt")

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

;; When we can see statically that the `or` must take the second branch,
;; the generated code specializes to simply return that value of `x`.
(run* (x v) (staged (evalo `(or #f x) `((x . ,x)) v)))
(generated-code)
#|
(lambda (x v) (disj (== v x)))
|#

;; Because the value of `x` may be true or false, we need to generate
;; code with a runtime branch on the value to implement the semantics of `or`.
(run* (x v) (staged (evalo `(or x #t) `((x . ,x)) v)))
(generated-code)
#|
(lambda (x v)
  (disj (conj (=/= x #f) (== v x))
        (conj (== x #f) (== v #t))))
|#


;; When the first expression is unknown, we can generate specialized miniKanren
;; code for the semantics of `or`, but need to invoke the runtime interpreter
;; to evaluate the unknown expression.
(run 3 (e v) (staged (evalo `(or ,e #f) '() v)))
(generated-code)
#|
(lambda (e v)
  (fresh (e-res)
    (invoke-fallback evalo/1 e '() e-res)
    (disj (conj (=/= v #f) (== e-res v))
          (conj (== e-res #f) (== v #f)))))
|#


;; For a lambda expression we want to generate one copy of specialized miniKanren code
;; that can then be invoked multiple times, once for each application. We achieve this
;; using partially applied relations.
(run* (e v) (staged (evalo `((lambda (f) (or (f #t) (f #f)))
                             (lambda (x) x)) '() v)))
(generated-code)
#|
(lambda (v)
  (fresh (lam1 lam2)
    (== lam1 (apply-rep 'applyo '(f (or (f #t) (f #f)) ())
                        (lambda (f out)
                          (fresh (f-res)
                            (finish-apply f applyo #t f-res)
                            (disj (conj (=/= out '#f) (== f-res out))
                                  (conj (== f-res '#f)
                                        (finish-apply f applyo #f out)))))))
    (== lam2 (apply-rep 'applyo '(x x ())
                        (lambda (x out) (== x out))))
    (finish-apply lam1 applyo (lam2 v))))
|#