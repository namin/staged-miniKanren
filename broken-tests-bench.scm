(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(load "staged-interp.scm")
(load "staged-utils.scm")

(load "unstaged-interp.scm")

(load "test-check.scm")

(define ho-quine-interp
  (eval
   (gen 'eval-expr '(expr)
        `(letrec ([eval-expr
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
                       [`(cons ,e1 ,e2)
                        (cons (eval-expr e1 env) (eval-expr e2 env))]
                       [`(,rator ,rand)
                        ((eval-expr rator env) (eval-expr rand env))]))])
           (eval-expr expr (lambda (y) 'error))))))

(time-test
  (run 1 (q)
    (absento 'error q)
    (ho-quine-interp q q))
  '???)
