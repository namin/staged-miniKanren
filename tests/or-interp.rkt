#lang racket
(require staged-miniKanren)

(defrel/staged (booleano e)
  (conde
   ((== e #t))
   ((== e #f))))

(defrel/staged (lookupo x e v)
  (fresh (y a env^)
    (== e `((,y . ,a) . ,env^))
    (conde
     ((== y x) (== a v))
     ((=/= y x) (lookupo x env^ v)))))


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
    ;; adding the lambda-calculus
    ((symbolo e) (lookupo e env v))
    ((fresh (x e0)
            (== e `(lambda (,x) ,e0))
            (specialize-partial-apply v applyo x e0 env)))
    ((fresh (e1 e2 v1 v2)
            (== e `(,e1 ,e2))
            (evalo e1 env v1)
            (evalo e2 env v2)
            (later (finish-apply v1 applyo v2 v)))))))

(run 1 (q v) (evalo '(or #f #t) q v))

(run 1 (q v) (staged (evalo `(or a #t) q v)))

(run 1 (q v e) (staged (evalo `(or a ,e) `((a . ,q)) v)))

(generated-code)