(load "mk/test-check.scm")
(load "evalo-standard.scm")
;; Using the optimized interpreter is slightly faster, but not necessary.
;(load "evalo-optimized.scm")
;(set! allow-incomplete-search? #t)

;; Tower of interpretation!
;;; Scheme
;;;; miniKanren written in Scheme
;;;;; Scheme interpreter written in miniKanren
;;;;;; Scheme interpreter (eval-expr) relationally interpreted
;;;;;;; Scheme program (,q) being interpreted by eval-expr  <-- WE ARE HERE!

(printf "Give this a few minutes.\n")
(time
  (test 'scheme-in-scheme-quine-with-quasiquote
    (run 1 (q)
      (evalo
        `(letrec ([eval-quasi
                    (lambda (q eval)
                      (match q
                        [(? symbol? x) x]
                        [`() '()]
                        [`(,,'`unquote ,exp) (eval exp)]
                        [`(quasiquote ,datum) ('error)]
                        [`(,a . ,d)
                          (cons (eval-quasi a eval)
                                (eval-quasi d eval))]))])
           ;; We use a nested letrec because our standard version of evalo does
           ;; not support mutually-recursive definitions, and we'd like to show
           ;; that it is capable of passing this test.  Our optimized version
           ;; of evalo supports mutual recursion.
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
                            (eval-quasi
                              datum
                              (lambda (exp) (eval-expr exp env)))]
                          [`(,rator ,rand)
                            ((eval-expr rator env)
                             (eval-expr rand env))]))])
             (eval-expr ',q
                        ;; 'initial-env represents the empty environment.  A
                        ;; syntactically correct program will never access it.
                        'initial-env)))
        q))
    '((((lambda (_.0) `(,_.0 ',_.0))
        '(lambda (_.0) `(,_.0 ',_.0)))
       (=/= ((_.0 closure)) ((_.0 prim))) (sym _.0)))))
