#lang racket/base

(require "../../all.rkt")

(record-bench 'staging 'eval-and-map-evalo)
(defrel (eval-and-map-evalo expr val)
  (time-staged
   (evalo-staged
    `(letrec ([map (lambda (f l)
                     (if (null? l)
                         '()
                         (cons (f (car l))
                               (map f (cdr l)))))])
       (letrec ([eval-expr
                 (lambda (expr env)
                   (match expr
                     [`(quote ,datum) datum]
                     [`(null? ,e)
                      (null? (eval-expr e env))]
                     [`(car ,e)
                      (car (eval-expr e env))]
                     [`(cdr ,e)
                      (cdr (eval-expr e env))]
                     [`(cons ,e1 ,e2)
                      (cons (eval-expr e1 env)
                            (eval-expr e2 env))]
                     [`(if ,e1 ,e2 ,e3)
                      (if (eval-expr e1 env)
                          (eval-expr e2 env)
                          (eval-expr e3 env))]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [(? symbol? x) (env x)]
                     [`(map ,e1 ,e2)
                      (map (eval-expr e1 env) (eval-expr e2 env))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))])
         (eval-expr ',expr (lambda (y) 'error))))
    val)))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (eval-and-map-evalo `(map ,q '(a b c)) '((a . a) (b . b) (c . c))))
  `(((lambda (_.0) (cons _.0 _.0))
     $$
     ,not-tags0+error
     (sym _.0))))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (eval-and-map-evalo `(map (lambda (x) ,q) '(a b c)) '((a . a) (b . b) (c . c))))
  '((cons x x)))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (eval-and-map-evalo `(map (lambda (x) ,q) '(a b c)) '((a . a) (b . b) (c . c))))
  '((cons x x)))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (eval-and-map-evalo `(cons (map (lambda (x) ,q) '(a))
                               (cons (map (lambda (x) ,q) '(b c))
                                     (cons (map (lambda (x) ,q) '(d e f))
                                           '())))
                        '(((a . a)) ((b . b) (c . c)) ((d . d) (e . e) (f . f)))))
  '((cons x x)))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (eval-and-map-evalo `(cons (map (lambda (x) ,q) '())
                               (cons (map (lambda (x) ,q) '(a))
                                     (cons (map (lambda (x) ,q) '(b c))
                                           (cons (map (lambda (x) ,q) '(d e f))
                                                 '()))))
                        '(() ((a . a)) ((b . b) (c . c)) ((d . d) (e . e) (f . f)))))
  '((cons x x)))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (eval-and-map-evalo `((lambda (proc)
                            (cons (map proc '())
                                  (cons (map proc '(a))
                                        (cons (map proc '(b c))
                                              (cons (map proc '(d e f))
                                                    '())))))
                          (lambda (x) ,q))
                        '(() ((a . a)) ((b . b) (c . c)) ((d . d) (e . e) (f . f)))))
  '((cons x x)))

(record-bench 'staged 'eval-and-map-evalo)
(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (eval-and-map-evalo `((lambda (proc)
                            (cons (map proc '())
                                  (cons (map proc '(a))
                                        (cons (map proc '(b c))
                                              (cons (map proc '(d e f))
                                                    '())))))
                          (lambda (x) ,q))
                        '(() ((a (a) a)) ((b (b) b) (c (c) c)) ((d (d) d) (e (e) e) (f (f) f)))))
  '((cons x (cons (cons x '()) (cons x '())))))

(record-bench 'unstaged 'eval-and-map-evalo)
(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (evalo-unstaged
     `(letrec ([map (lambda (f l)
                      (if (null? l)
                          '()
                          (cons (f (car l))
                                (map f (cdr l)))))])
        (letrec ([eval-expr
                  (lambda (expr env)
                    (match expr
                      [`(quote ,datum) datum]
                      [`(null? ,e)
                       (null? (eval-expr e env))]
                      [`(car ,e)
                       (car (eval-expr e env))]
                      [`(cdr ,e)
                       (cdr (eval-expr e env))]
                      [`(cons ,e1 ,e2)
                       (cons (eval-expr e1 env)
                             (eval-expr e2 env))]
                      [`(if ,e1 ,e2 ,e3)
                       (if (eval-expr e1 env)
                           (eval-expr e2 env)
                           (eval-expr e3 env))]
                      [`(lambda (,(? symbol? x)) ,body)
                       (lambda (a)
                         (eval-expr body (lambda (y)
                                           (if (equal? x y)
                                               a
                                               (env y)))))]
                      [(? symbol? x) (env x)]
                      [`(map ,e1 ,e2)
                       (map (eval-expr e1 env) (eval-expr e2 env))]
                      [`(,rator ,rand)
                       ((eval-expr rator env) (eval-expr rand env))]))])
          (eval-expr '((lambda (proc)
                         (cons (map proc '())
                               (cons (map proc '(a))
                                     (cons (map proc '(b c))
                                           (cons (map proc '(d e f))
                                                 '())))))
                       (lambda (x) ,q)) (lambda (y) 'error))))
     '(() ((a (a) a)) ((b (b) b) (c (c) c)) ((d (d) d) (e (e) e) (f (f) f)))))
  '((cons x (cons (cons x '()) (cons x '())))))

(define-term-syntax-rule (eval-and-map-and-list-eval letrec-body)
  `(letrec ([map (lambda (f l)
                         (if (null? l)
                             '()
                             (cons (f (car l))
                                   (map f (cdr l)))))])
           (letrec ([eval-expr
                     (lambda (expr env)
                       (match expr
                         [`(quote ,datum) datum]
                         [`(null? ,e)
                          (null? (eval-expr e env))]
                         [`(car ,e)
                          (car (eval-expr e env))]
                         [`(cdr ,e)
                          (cdr (eval-expr e env))]
                         [`(cons ,e1 ,e2)
                          (cons (eval-expr e1 env)
                                (eval-expr e2 env))]
                         [`(if ,e1 ,e2 ,e3)
                          (if (eval-expr e1 env)
                              (eval-expr e2 env)
                              (eval-expr e3 env))]
                         [`(lambda (,(? symbol? x)) ,body)
                          (lambda (a)
                            (eval-expr body (lambda (y)
                                              (if (equal? x y)
                                                  a
                                                  (env y)))))]
                         [(? symbol? x) (env x)]
                         [`(map ,e1 ,e2)
                          (map (eval-expr e1 env) (eval-expr e2 env))]
                         [`(list . ,e*)
                          (map (lambda (e) (eval-expr e env)) e*)]
                         [`(,rator ,rand)
                          ((eval-expr rator env) (eval-expr rand env))]))])
             ,letrec-body)))

(record-bench 'staging 'eval-and-map-and-list-evalo)
(defrel (eval-and-map-and-list-evalo expr val)
  (time-staged
   (evalo-staged
    (eval-and-map-and-list-eval `(eval-expr ',expr (lambda (y) 'error)))
    val)))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (eval-and-map-and-list-evalo `((lambda (proc)
                                     (list (map proc '())
                                           (map proc '(a))
                                           (map proc '(b c))
                                           (map proc '(d e f))))
                                   (lambda (x) ,q))
                        '(() ((a . a)) ((b . b) (c . c)) ((d . d) (e . e) (f . f)))))
  '((cons x x)))

(record-bench 'staged 'eval-and-map-and-list-evalo)
(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (eval-and-map-and-list-evalo `((lambda (proc)
                                     (list (map proc '())
                                           (map proc '(a))
                                           (map proc '(b c))
                                           (map proc '(d e f))))
                                   (lambda (x) ,q))
                        '(() ((a (a) a)) ((b (b) b) (c (c) c)) ((d (d) d) (e (e) e) (f (f) f)))))
  '((cons x (cons (cons x '()) (cons x '())))))

(record-bench 'unstaged 'eval-and-map-and-list-evalo)
(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (evalo-unstaged
     (eval-and-map-and-list-eval
      `(eval-expr '((lambda (proc)
                      (list (map proc '())
                            (map proc '(a))
                            (map proc '(b c))
                            (map proc '(d e f))))
                   (lambda (x) ,q)) (lambda (y) 'error)))
     '(() ((a (a) a)) ((b (b) b) (c (c) c)) ((d (d) d) (e (e) e) (f (f) f)))))
  '((list x (cons x '()) x)))


#|
;;;  Painfully slow to generate this code!  Does it even terminate?
(define eval-and-map-and-list-and-let-evalo
  (eval
   (gen 'eval-expr '(expr)
        `(letrec ([map (lambda (f l)
                         (if (null? l)
                             '()
                             (cons (f (car l))
                                   (map f (cdr l)))))])
           (letrec ([eval-expr
                     (lambda (expr env)
                       (match expr
                         [`(quote ,datum) datum]
                         [`(null? ,e)
                          (null? (eval-expr e env))]
                         [`(car ,e)
                          (car (eval-expr e env))]
                         [`(cdr ,e)
                          (cdr (eval-expr e env))]
                         [`(cons ,e1 ,e2)
                          (cons (eval-expr e1 env)
                                (eval-expr e2 env))]
                         [`(if ,e1 ,e2 ,e3)
                          (if (eval-expr e1 env)
                              (eval-expr e2 env)
                              (eval-expr e3 env))]
                         [`(lambda (,(? symbol? x)) ,body)
                          (lambda (a)
                            (eval-expr body (lambda (y)
                                              (if (equal? x y)
                                                  a
                                                  (env y)))))]
                         [(? symbol? x) (env x)]
                         [`(map ,e1 ,e2)
                          (map (eval-expr e1 env) (eval-expr e2 env))]
                         [`(let ([,(? symbol? x) ,e]) ,body)
                          ((lambda (a)
                             (eval-expr body (lambda (y)
                                               (if (equal? x y)
                                                   a
                                                   (env y)))))
                           (eval-expr e env))]
                         [`(list . ,e*)
                          (map (lambda (e) (eval-expr e env)) e*)]
                         [`(,rator ,rand)
                          ((eval-expr rator env) (eval-expr rand env))]))])
             (eval-expr expr (lambda (y) 'error)))))))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (eval-and-map-and-list-and-let-evalo `(let ((proc (lambda (x) ,q)))
                                            (list (map proc '())
                                                  (map proc '(a))
                                                  (map proc '(b c))
                                                  (map proc '(d e f))))
                        '(() ((a . a)) ((b . b) (c . c)) ((d . d) (e . e) (f . f)))))
  '((cons x x)))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (eval-and-map-and-list-and-let-evalo `(let ((proc (lambda (x) ,q)))
                                            (list (map proc '())
                                                  (map proc '(a))
                                                  (map proc '(b c))
                                                  (map proc '(d e f))))
                        '(() ((a (a) a)) ((b (b) b) (c (c) c)) ((d (d) d) (e (e) e) (f (f) f)))))
  '((list x (cons x '()) x)))
|#

(define-term-syntax-rule (quasi-quine-eval initial-expr)
   `(letrec ([eval-quasi (lambda (q eval)
                           (match q
                             [(? symbol? x) x]
                             [`() '()]
                             [`(,,'`unquote ,exp) (eval exp)]
                             ;;[`(,`unquote ,exp) (eval exp)]
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

(record-bench 'staging 'quasi-quine-evalo)
(defrel (quasi-quine-evalo expr val)
  (time-staged
   (evalo-staged (quasi-quine-eval expr) val)))

(record-bench 'staged 'quasi-quine-evalo)
(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (quasi-quine-evalo q q))
  `((((lambda (_.0) `(,_.0 ',_.0)) '(lambda (_.0) `(,_.0 ',_.0)))
     $$
     ,not-tags0+error
     (sym _.0))))

(record-bench 'run-staged 'quasi-quine-evalo)
(time-test
  (run 1 (q)
    (absento 'error q)
    (absento 'struct q)
    (staged (evalo-staged (quasi-quine-eval q) q)))
  `((((lambda (_.0) `(,_.0 ',_.0)) '(lambda (_.0) `(,_.0 ',_.0)))
     $$
     ,not-tags0+error
     (sym _.0))))

(record-bench 'unstaged 'quasi-quine-evalo)
(time-test
 (run 1 (q)
   (absento 'error q)
   (absento 'struct q)
   (evalo-unstaged (quasi-quine-eval q) q))
 `((((lambda (_.0) `(,_.0 ',_.0)) '(lambda (_.0) `(,_.0 ',_.0)))
    $$
    ,not-tags0+error
    (sym _.0))))


(define-term-syntax-rule (ho-quine-interp-cons-fun letrec-body)
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
             ,letrec-body))

(record-bench 'staging 'ho-quine-interp-cons)
(defrel (ho-quine-interp-cons expr val)
  (time-staged
   (evalo-staged
    (ho-quine-interp-cons-fun `(eval-expr ',expr (lambda (y) 'error)))
    val)))

(record-bench 'staged 'ho-quine-interp-cons)
(time-test
  (run 1 (q)
    (absento 'error q)
    (absento 'struct q)
    (ho-quine-interp-cons q q))
  `((((lambda (_.0) (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0) (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     $$
     ,not-tags0+error
     (sym _.0))))

(record-bench 'unstaged 'ho-quine-interp-cons)
(time-test
  (run 1 (q)
    (absento 'error q)
    (absento 'struct q)
    (evalo-unstaged
     (ho-quine-interp-cons-fun `(eval-expr ',q (lambda (y) 'error)))
     q))
  `((((lambda (_.0) (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0) (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     $$
     ,not-tags0+error
     (sym _.0))))

(define-term-syntax-rule (ho-double-eval letrec-body)
          `(letrec ([eval-expr
                   (lambda (expr env)
                     (match expr
                       [`(quote ,datum) datum]
                       [`(list . ,e*)
                        (letrec ([f (lambda (e*)
                                      (match e*
                                        [`() '()]
                                        [`(,e . ,rest)
                                         (cons (eval-expr e env) (f rest))]))])
                          (f e*))]
                       [`(lambda (,(? symbol? x)) ,body)
                        (lambda (a)
                          (eval-expr body (lambda (y)
                                            (if (equal? x y)
                                                a
                                                (env y)))))]
                       [(? symbol? x) (env x)]
                       [`(,rator ,rand)
                        ((eval-expr rator env)
                         (eval-expr rand env))]
                       ))])
             ,letrec-body))

(record-bench 'staging 'ho-double-evalo)
(defrel (ho-double-evalo expr val)
  (time-staged
    (evalo-staged
     (ho-double-eval `(eval-expr ',expr (lambda (y) 'error)))
     val)))

(time-test
  (run 1 (q) (ho-double-evalo '((lambda (x) x) 'hello) q))
  '(hello))

(record-bench 'staged 'ho-double-evalo)
(time-test
 (run 1 (q) (absento 'error q) (absento 'struct q) (ho-double-evalo q q))
 `((((lambda (_.0) (list _.0 (list 'quote _.0)))
    '(lambda (_.0) (list _.0 (list 'quote _.0))))
   $$
   ,not-tags0+error
   (sym _.0))))

(record-bench 'unstaged 'ho-double-evalo)
(time-test
 (run 1 (q)
   (absento 'error q)
   (absento 'struct q)
      (evalo-unstaged
       (ho-double-eval `(eval-expr ',q (lambda (y) 'error)))
       q))
 `((((lambda (_.0) (list _.0 (list 'quote _.0)))
    '(lambda (_.0) (list _.0 (list 'quote _.0))))
   $$
   ,not-tags0+error
   (sym _.0))))

#|
#lang racket

(letrec ([lookup
          (lambda (x env)
            (match env
              [`((,y . ,v) . ,renv)
               (if (equal? x y)
                   v
                   (lookup x renv))]))]
         [eval-expr
          (lambda (expr env)
            (match expr
              [`(quote ,datum) datum]
              [`(null? ,e)
               (null? (eval-expr e env))]
              [`(car ,e)
               (car (eval-expr e env))]
              [`(cdr ,e)
               (cdr (eval-expr e env))]
              [`(cons ,e1 ,e2)
               (cons (eval-expr e1 env)
                     (eval-expr e2 env))]
              [`(if ,e1 ,e2 ,e3)
               (if (eval-expr e1 env)
                   (eval-expr e2 env)
                   (eval-expr e3 env))]
              [`(lambda (,(? symbol? x)) ,body)
               (list 'clo x body env)]
              [(? symbol? x) (lookup x env)]
              [`(,rator ,rand)
               (match (eval-expr rator env)
                 [`(clo ,x ,body ,clo-env)
                  (eval-expr body (cons (cons x (eval-expr rand env)) clo-env))])]))])
  (eval-expr `((lambda (f)
                 (((f f)
                   (lambda (x) (cons x x)))
                  '(a b c)))
               (lambda (f)
                 (lambda (f^)
                   (lambda (l)
                     (if (null? l)
                         '()
                         (cons (f^ (car l)) (((f f) f^) (cdr l))))))))
             '()))

;; => ((a . a) (b . b) (c . c))
|#

(define-term-syntax-rule (map-in-double-eval-fun letrec-body)
          `(letrec ([lookup
                   (lambda (x env)
                     (match env
                       [`((,y . ,v) . ,renv)
                        (if (equal? x y)
                            v
                            (lookup x renv))]))])
             (letrec ([eval-expr
                   (lambda (expr env)
                     (match expr
                       [`(quote ,datum) datum]
                       [`(null? ,e)
                        (null? (eval-expr e env))]
                       [`(car ,e)
                        (car (eval-expr e env))]
                       [`(cdr ,e)
                        (cdr (eval-expr e env))]
                       [`(cons ,e1 ,e2)
                        (cons (eval-expr e1 env)
                              (eval-expr e2 env))]
                       [`(if ,e1 ,e2 ,e3)
                        (if (eval-expr e1 env)
                            (eval-expr e2 env)
                            (eval-expr e3 env))]
                       [`(lambda (,(? symbol? x)) ,body)
                        (list 'clo x body env)]
                       [(? symbol? x) (lookup x env)]
                       [`(,rator ,rand)
                        (match (eval-expr rator env)
                          [`(clo ,x ,body ,clo-env)
                           (eval-expr body (cons (cons x (eval-expr rand env)) clo-env))])]))])
             ,letrec-body)))
(record-bench 'staging 'map-in-double-eval)
(defrel (map-in-double-eval expr val)
  (time-staged
    (evalo-staged
     (map-in-double-eval-fun `(eval-expr ',expr '()))
     val)))

(time-test
 (run 1 (q)
   (fresh (expr)
    (absento 'clo q)
    (== '((lambda (f)
            (((f f)
              (lambda (x) (cons x x)))
             '(a b c)))
          (lambda (f)
            (lambda (f^)
              (lambda (l)
                (if (null? l)
                    '()
                    (cons (f^ (car l)) (((f f) f^) (cdr l))))))))
        expr)
    (map-in-double-eval expr q)))
 '(((a . a) (b . b) (c . c))))

(record-bench 'staged 'map-in-double-eval)
(time-test
 (run 1 (q)
   (fresh (expr)
    (absento 'clo q)
    (== `((lambda (f)
            (((f f)
              (lambda (x) ,q))
             '(a b c)))
          (lambda (f)
            (lambda (f^)
              (lambda (l)
                (if (null? l)
                    '()
                    (cons (f^ (car l)) (((f f) f^) (cdr l))))))))
        expr)
    (map-in-double-eval expr '((a . a) (b . b) (c . c)))))
 '((cons x x)))

#|
(record-bench 'unstaged 'map-in-double-eval)
(time-test
 (run 1 (q)
   (fresh (expr)
    (absento 'clo q)
    (== `((lambda (f)
            (((f f)
              (lambda (x) ,q))
             '(a b c)))
          (lambda (f)
            (lambda (f^)
              (lambda (l)
                (if (null? l)
                    '()
                    (cons (f^ (car l)) (((f f) f^) (cdr l))))))))
        expr)
    (evalo-unstaged
     (map-in-double-eval-fun `(eval-expr ,expr '()))
     '((a . a) (b . b) (c . c)))))
 '((cons x x)))
|#

(record-bench 'staging 'double-evalo)
(defrel (double-evalo expr val)
  (time-staged
   (evalo-staged
    `(letrec ([lookup
               (lambda (x env)
                 (match env
                   [`((,y . ,v) . ,renv)
                    (if (equal? x y)
                        v
                        (lookup x renv))]))])
       (letrec ([eval-expr
                 (lambda (expr env)
                   (match expr
                     [`(quote ,datum) datum]
                     [`(lambda (,(? symbol? x)) ,body)
                      (list 'clo x body env)]
                     [`(list ,e1 ,e2)
                      (list (eval-expr e1 env) (eval-expr e2 env))]
                     [(? symbol? x) (lookup x env)]
                     [`(,rator ,rand)
                      (match (eval-expr rator env)
                        [`(clo ,x ,body ,clo-env)
                         (eval-expr body (cons (cons x (eval-expr rand env)) clo-env))])]))])
         (eval-expr ',expr '())))
    val))
  )

(record-bench 'staged 'double-evalo)
(time-test
 (run 1 (q) (absento 'clo q) (double-evalo q q))
 `((((lambda (_.0) (list _.0 (list 'quote _.0)))
     '(lambda (_.0) (list _.0 (list 'quote _.0))))
    $$
    ,not-tags0+clo
    (sym _.0))))

(record-bench 'unstaged 'double-evalo)
(time-test
  (run 1 (q)
    (fresh (expr)
      (absento 'clo expr)
      (== q expr)
      (evalo-unstaged
       `(letrec ([lookup
                  (lambda (x env)
                    (match env
                      [`((,y . ,v) . ,renv)
                       (if (equal? x y)
                           v
                           (lookup x renv))]))])
          (letrec ([eval-expr
                    (lambda (expr env)
                      (match expr
                        [`(quote ,datum) datum]
                        [`(lambda (,(? symbol? x)) ,body)
                         (list 'clo x body env)]
                        [`(list ,e1 ,e2)
                         (list (eval-expr e1 env) (eval-expr e2 env))]
                        [(? symbol? x) (lookup x env)]
                        [`(,rator ,rand)
                         (match (eval-expr rator env)
                           [`(clo ,x ,body ,clo-env)
                            (eval-expr body (cons (cons x (eval-expr rand env)) clo-env))])]))])
            (eval-expr ',expr '())))
       q)))
  `((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     $$
     ,not-tags0+clo
     (sym _.0))))

(define-term-syntax-rule (double-evalo-variadic-list-fo-fun letrec-body)
          `(letrec ([lookup
                   (lambda (x env)
                     (match env
                       [`((,y . ,v) . ,renv)
                        (if (equal? x y)
                            v
                            (lookup x renv))]))])
             (letrec ([eval-expr
                   (lambda (expr env)
                     (match expr
                       [`(quote ,datum) datum]
                       [`(lambda (,(? symbol? x)) ,body)
                        (list 'clo x body env)]
                       [`(list . ,e*)
                        (match e*
                          [`() '()]
                          [`(,e . ,rest)
                           (cons (eval-expr e env) (eval-expr (cons 'list rest) env))])]
                       [(? symbol? x) (lookup x env)]
                       [`(,rator ,rand)
                        (match (eval-expr rator env)
                          [`(clo ,x ,body ,clo-env)
                           (eval-expr body (cons (cons x (eval-expr rand env)) clo-env))])]))])
             ,letrec-body)))

(record-bench 'staging 'double-evalo-variadic-list-fo)
(defrel (double-evalo-variadic-list-fo expr val)
  (time-staged
    (evalo-staged
     (double-evalo-variadic-list-fo-fun `(eval-expr ',expr '()))
     val)))

(record-bench 'staged 'double-evalo-variadic-list-fo)
(time-test
  (run 1 (q) (absento 'clo q) (double-evalo-variadic-list-fo q q))
  `((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     $$
     ,not-tags0+clo
     (sym _.0))))

(record-bench 'unstaged 'double-evalo-variadic-list-fo)
(time-test
 (run 1 (q) (absento 'clo q)
      (evalo-unstaged
       (double-evalo-variadic-list-fo-fun `(eval-expr ',q '())) q))
 `((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     $$
     ,not-tags0+clo
     (sym _.0))))

(define-term-syntax-rule (double-evalo-variadic-list-fo-less-ridiculous-fun letrec-body)
          `(letrec ([lookup
                   (lambda (x env)
                     (match env
                       [`((,y . ,v) . ,renv)
                        (if (equal? x y)
                            v
                            (lookup x renv))]))])
             (letrec ([eval-expr
                   (lambda (expr env)
                     (match expr
                       [`(quote ,datum) datum]
                       [`(lambda (,(? symbol? x)) ,body)
                        (list 'clo x body env)]
                       [`(list . ,e*)
                        (letrec ([f (lambda (e*)
                                      (match e*
                                        [`() '()]
                                        [`(,e . ,rest)
                                         (cons (eval-expr e env) (f rest))]))])
                          (f e*))]
                       [(? symbol? x) (lookup x env)]
                       [`(,rator ,rand)
                        (match (eval-expr rator env)
                          [`(clo ,x ,body ,clo-env)
                           (eval-expr body (cons (cons x (eval-expr rand env)) clo-env))])]))])
             ,letrec-body)))
(record-bench 'staging 'double-evalo-variadic-list-fo-better)
(defrel (double-evalo-variadic-list-fo-less-ridiculous expr val)
  (time-staged
    (evalo-staged
     (double-evalo-variadic-list-fo-less-ridiculous-fun `(eval-expr ',expr '()))
     val)))

(record-bench 'staged 'double-evalo-variadic-list-fo-better)
(time-test
  (run 1 (q) (absento 'clo q) (double-evalo-variadic-list-fo-less-ridiculous q q))
  `((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     $$
     ,not-tags0+clo
     (sym _.0))))

(record-bench 'unstaged 'double-evalo-variadic-list-fo-better)
(time-test
 (run 1 (q) (absento 'clo q)
      (evalo-unstaged
       (double-evalo-variadic-list-fo-less-ridiculous-fun `(eval-expr ',q '())) q))
 `((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     $$
     ,not-tags0+clo
     (sym _.0))))

(define-term-syntax-rule (double-evalo-variadic-list-ho-fun letrec-body)
  `(letrec ([lookup
                   (lambda (x env)
                     (match env
                       [`((,y . ,v) . ,renv)
                        (if (equal? x y)
                            v
                            (lookup x renv))]))])
     (letrec ([map
                   (lambda (f l)
                     (match l
                       [`() '()]
                       [`(,a . ,d)
                        (cons (f a) (map f d))]))])
       (letrec ([eval-expr
                   (lambda (expr env)
                     (match expr
                       [`(quote ,datum) datum]
                       [`(lambda (,(? symbol? x)) ,body)
                        (list 'clo x body env)]
                       [`(list . ,e*)
                        (map (lambda (e) (eval-expr e env)) e*)]
                       [(? symbol? x) (lookup x env)]
                       [`(,rator ,rand)
                        (match (eval-expr rator env)
                          [`(clo ,x ,body ,clo-env)
                           (eval-expr body (cons (cons x (eval-expr rand env)) clo-env))])]))])
     ,letrec-body))))

(record-bench 'staging 'double-evalo-variadic-list-ho)
(defrel (double-evalo-variadic-list-ho expr val)
  (time-staged
    (evalo-staged
     (double-evalo-variadic-list-ho-fun `(eval-expr ',expr '()))
     val)))

(record-bench 'staged 'double-evalo-variadic-list-ho)
(time-test
  (run 1 (q) (absento 'clo q) (double-evalo-variadic-list-ho q q))
  `((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
   $$
   ,not-tags0+clo
   (sym _.0))))

(record-bench 'unstaged 'double-evalo-variadic-list-ho)
(time-test
 (run 1 (q) (absento 'clo q)
      (evalo-unstaged
       (double-evalo-variadic-list-ho-fun `(eval-expr ',q '()))
       q))
 `((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
   $$
   ,not-tags0+clo
   (sym _.0))))

(define-term-syntax-rule (double-evalo-cons-fun letrec-body)
          `(letrec ([lookup
                   (lambda (x env)
                     (match env
                       [`((,y . ,v) . ,renv)
                        (if (equal? x y)
                            v
                            (lookup x renv))]))])
             (letrec ([eval-expr
                   (lambda (expr env)
                     (match expr
                       [`(quote ,datum) datum]
                       [`(lambda (,(? symbol? x)) ,body)
                        (list 'clo x body env)]
                       [`(cons ,e1 ,e2)
                        (cons (eval-expr e1 env) (eval-expr e2 env))]
                       [(? symbol? x) (lookup x env)]
                       [`(,rator ,rand)
                        (match (eval-expr rator env)
                          [`(clo ,x ,body ,clo-env)
                           (eval-expr body (cons (cons x (eval-expr rand env)) clo-env))])]))])
             ,letrec-body)))

(record-bench 'staging 'double-evalo-cons)
(defrel (double-evalo-cons expr val)
  (time-staged
    (evalo-staged
     (double-evalo-cons-fun `(eval-expr ',expr '()))
     val)))

(record-bench 'staged 'double-evalo-cons)
(time-test
  (run 1 (q) (absento 'clo q) (double-evalo-cons q q))
  `((((lambda (_.0)
        (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0)
         (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     $$
     ,not-tags0+clo
     (sym _.0))))

(record-bench 'unstaged 'double-evalo-cons)
(time-test
 (run 1 (q) (absento 'clo q)
      (evalo-unstaged
       (double-evalo-cons-fun `(eval-expr ',q '()))
       q))
 `((((lambda (_.0)
        (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0)
         (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     $$
     ,not-tags0+clo
     (sym _.0))))
