#lang racket

(require "../../../all.rkt")

(define-term-syntax-rule (quasi-quine-evalo initial-expr)
  `(letrec ([eval-quasi (lambda (q eval)
                          (match q
                            [(? symbol? x) x]
                            [`() '()]
                            [`(,,'`unquote ,exp) (eval exp)]  ;; Racket verison
                            ;;[`(,`unquote ,exp) (eval exp)]  ;; Chez version
                            [`(quasiquote ,datum) 'error]
                            ;; ('error) in the 2017 ICFP Pearl, but
                            ;; the code generator rejects this erroneous code!
                            [`(,a . ,d)
                             (cons (eval-quasi a eval) (eval-quasi d eval))]))])
     (letrec ([eval-expr
               (lambda (expr env)
                 (match expr
                   [`(quote ,datum) datum]
                   [`(lambda (,(? symbol? x)) ,body)
                    (lambda (a)
                      (eval-expr body (lambda (y)
                                        (if (equal? x y)
                                            a
                                            (env y)))))]
                   [(? symbol? x) (env x)]
                   [`(quasiquote ,datum)
                    (eval-quasi datum (lambda (exp) (eval-expr exp env)))]
                   [`(,rator ,rand)
                    ((eval-expr rator env) (eval-expr rand env))]))])
       (eval-expr ',initial-expr (lambda (y) 'error)))))

(defrel (quasi-quine-evalo-unstaged expr val)
  (evalo-unstaged (quasi-quine-evalo expr) val))

(defrel (quasi-quine-evalo-staged expr val)
  (time-staged
   (evalo-staged (quasi-quine-evalo expr) val)))
#;(generated-code)

;; Unstaged takes ~25 seconds
(time
 (run 1 (q)
   (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
   (absento 'struct q)
   (quasi-quine-evalo-unstaged q q)))
   
(time
 (run 1 (q)
   (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
   (absento 'struct q)
   (quasi-quine-evalo-staged q q)))
