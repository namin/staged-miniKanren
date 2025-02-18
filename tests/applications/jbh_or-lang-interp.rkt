#lang racket/base

(require "../../all.rkt")

(record-bench 'eval-eval 'staging 'eval-or)
(defrel (or-evalo expr val)
  (time-staged
    (evalo-staged
      `(letrec ([eval-or
         (lambda (expr env)
           (match expr
		     [#t #t]
		     [#f #f]
             [`(or ,e1 ,e2)
               (match (eval-or e1 env)
				 [#f (eval-or e2 env)]
				 [v v])]
             [(? symbol? x) (env x)]
             [`(lambda (,(? symbol? x)) ,body)
               (lambda (a)
                 (eval-or body (lambda (y)
                                 (if (equal? x y)
                                     a
                                     (env y)))))]
             [`(,rator ,rand)
              ((eval-or rator env) (eval-or rand env))]))])
          (eval-or ',expr (lambda (y) 'error)))
     val)))

;; ~/Documents/staged-mk-paper/paper-code/staged-boolean-eval-interp.rkt
;; (run 90 (p r) (or-evalo p r))
;; The original bench has 90; 9 will suffice.
(run 9 (p r) (or-evalo p r))

(run 1 (v) (or-evalo '(or #f #t) v))

;; Do not b/c presently don't expose environment.
#;(run 1 (q v) (staged (evalo-or-staged `(or a #t) q v)))
#;(run 1 (q v e) (staged (evalo-or-staged `(or a ,e) `((a . ,q)) v)))
#;(run* (x v) (staged (evalo-or-staged `(or #f x) `((x . ,x)) v)))

(run 2 (q v) (or-evalo `((lambda (x) (or x x)) ,q) v))

#|
BENCH staging eval-or
cpu time: 12924 real time: 9183 gc time: 1704
'(((_.0 error) $$ (=/= ((_.0 struct))) (sym _.0)))
|#

