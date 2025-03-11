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

(defrel/staged (evalo-or-staged e env v)
  (fallback
   (conde
     ((booleano e) (== e v))
     ((fresh (e1 e2 v1)
        (== e `(or ,e1 ,e2))
        (evalo-or-staged e1 env v1)
        (gather
         (conde
           [(== v1 #f) (evalo-or-staged e2 env v)]
           [(=/= v1 #f) (== v1 v)]))))
     ((symbolo e) (lookupo e env v))
     ((fresh (x e0)
        (== e `(lambda (,x) ,e0))
        (symbolo x)
        (make-closo x e0 env v)))
     ((fresh (e1 e2 v1 v2)
        (== e `(,e1 ,e2))
        (evalo-or-staged e1 env v1)
        (evalo-or-staged e2 env v2)
        (later (apply-closo v1 v2 v)))))))

(defrel-partial/staged (applyo rep [x e env] [v1 v])
  (evalo-or-staged e `((,x . ,v1) . ,env) v))

(defrel/staged (make-closo x e env clos)
  (specialize-partial-apply clos applyo x e env))

(defrel (apply-closo clos v1 v)
  (finish-apply clos applyo v1 v))

(run 1 (q v) (evalo-or-staged '(or #f #t) q v))

(run 1 (q v) (staged (evalo-or-staged `(or a #t) q v)))

(run 1 (q v e) (staged (evalo-or-staged `(or a ,e) `((a . ,q)) v)))

(generated-code)

(run 2 (q v e) (staged (evalo-or-staged `((lambda (x) (or x x)) ,q) '() v)))

(generated-code)

(run* (x v) (staged (evalo-or-staged `(or #f x) `((x . ,x)) v)))

(generated-code)
