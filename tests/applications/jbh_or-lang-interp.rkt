#lang racket/base

(require "../../all.rkt")

(record-bench 'eval-eval 'staging 'eval-or)
(defrel (or-evalo-staged expr val)
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

;; Do not b/c presently don't expose environment.
#;(run 1 (q v) (staged (evalo-or-staged `(or a #t) q v)))
#;(run 1 (q v e) (staged (evalo-or-staged `(or a ,e) `((a . ,q)) v)))
#;(run* (x v) (staged (evalo-or-staged `(or #f x) `((x . ,x)) v)))

(record-bench 'eval-eval 'staged 'eval-or 1)
(time-test
  #:times 1000
  (run 2 (q v) (or-evalo-staged `((lambda (x) (or x x)) ,q) v))
  '((#t #t) (#f #f)))


(defrel (or-evalo-unstaged expr val)
  (evalo-unstaged
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
   val))

(record-bench 'eval-eval 'unstaged 'eval-or 1)
(time-test
  #:times 1000
  (run 2 (q v) (or-evalo-unstaged `((lambda (x) (or x x)) ,q) v))
  '((#t #t) (#f #f)))

;; ~/Documents/staged-mk-paper/paper-code/staged-boolean-eval-interp.rkt

(record-bench 'eval-eval 'staged 'eval-or 2)
(time-test
  #:times 1000
  (run 5 (p r) (or-evalo-staged p '#t))
  '((#t _.0) (((or #t _.0) _.1) $$ (absento (struct _.0))) ((or #f #t) _.0) (((or (or #t _.0) _.1) _.2) $$ (absento (struct _.0) (struct _.1))) (((or #f (or #t _.0)) _.1) $$ (absento (struct _.0)))))

(record-bench 'eval-eval 'unstaged 'eval-or 2)
(time-test
  #:times 1000
  (run 5 (p r) (or-evalo-unstaged p '#t))
  '((#t _.0) (((or #t _.0) _.1) $$ (absento (struct _.0))) ((or #f #t) _.0) (((or (or #t _.0) _.1) _.2) $$ (absento (struct _.0) (struct _.1))) (((or #f (or #t _.0)) _.1) $$ (absento (struct _.0)))))

(test
(run 1 (v) (or-evalo-staged '(or #f #t) v))
'(#t))
