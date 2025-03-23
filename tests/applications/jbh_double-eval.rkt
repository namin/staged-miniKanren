#lang racket/base

(require "../../all.rkt")

(defrel (eval-and-map-evalo expr val)
  (staged
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

(defrel (eval-and-map-evalo-unstaged expr val)
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
         (eval-expr ',expr (lambda (y) 'error))))
    val))


(test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (eval-and-map-evalo `(map ,q '(a b c)) '((a . a) (b . b) (c . c))))
  `(((lambda (_.0) (cons _.0 _.0))
     $$
     ,not-tags0+error
     (sym _.0))))

(test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (eval-and-map-evalo-unstaged `(map ,q '(a b c)) '((a . a) (b . b) (c . c))))
  `(((lambda (_.0) (cons _.0 _.0))
     $$
     ,not-tags0+error
     (sym _.0))))

;; Synthesize the body of a simple function call in map. Remember the interpreter implements map in terms of a host-language function map (the letrec there) that's all interpreted in evalo
(test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (eval-and-map-evalo `(map (lambda (x) ,q) '(a b c)) '((a . a) (b . b) (c . c))))
  '((cons x x)))


(test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (eval-and-map-evalo-unstaged `(map (lambda (x) ,q) '(a b c)) '((a . a) (b . b) (c . c))))
  '((cons x x)))


(test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (eval-and-map-evalo `(map (lambda (x) ,q) '(a b c)) '((a . a) (b . b) (c . c))))
  '((cons x x)))


(test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (eval-and-map-evalo-unstaged `(map (lambda (x) ,q) '(a b c)) '((a . a) (b . b) (c . c))))
  '((cons x x)))


(test
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


(test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (eval-and-map-evalo-unstaged `(cons (map (lambda (x) ,q) '(a))
                               (cons (map (lambda (x) ,q) '(b c))
                                     (cons (map (lambda (x) ,q) '(d e f))
                                           '())))
                        '(((a . a)) ((b . b) (c . c)) ((d . d) (e . e) (f . f)))))
  '((cons x x)))


(test
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


(test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (eval-and-map-evalo-unstaged `(cons (map (lambda (x) ,q) '())
                               (cons (map (lambda (x) ,q) '(a))
                                     (cons (map (lambda (x) ,q) '(b c))
                                           (cons (map (lambda (x) ,q) '(d e f))
                                                 '()))))
                        '(() ((a . a)) ((b . b) (c . c)) ((d . d) (e . e) (f . f)))))
  '((cons x x)))



(test
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


(test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (eval-and-map-evalo-unstaged `((lambda (proc)
                            (cons (map proc '())
                                  (cons (map proc '(a))
                                        (cons (map proc '(b c))
                                              (cons (map proc '(d e f))
                                                    '())))))
                          (lambda (x) ,q))
                        '(() ((a . a)) ((b . b) (c . c)) ((d . d) (e . e) (f . f)))))
  '((cons x x)))


;; synthesize the body of a somewhat complicated higher-order function that is used multiple times over in several calls to map that all produce a list of outputs
(test
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


(test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (eval-and-map-evalo-unstaged `((lambda (proc)
                            (cons (map proc '())
                                  (cons (map proc '(a))
                                        (cons (map proc '(b c))
                                              (cons (map proc '(d e f))
                                                    '())))))
                          (lambda (x) ,q))
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

(record-bench 'eval-eval 'staging 'list-eval)
(defrel (eval-and-map-and-list-evalo expr val)
  (time-staged
   (evalo-staged
    (eval-and-map-and-list-eval `(eval-expr ',expr (lambda (y) 'error)))
    val)))

(defrel (eval-and-map-and-list-evalo-unstaged expr val)
  (evalo-unstaged
   (eval-and-map-and-list-eval `(eval-expr ',expr (lambda (y) 'error)))
   val))

(test
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

(test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (eval-and-map-and-list-evalo-unstaged `((lambda (proc)
                                     (list (map proc '())
                                           (map proc '(a))
                                           (map proc '(b c))
                                           (map proc '(d e f))))
                                   (lambda (x) ,q))
                        '(() ((a . a)) ((b . b) (c . c)) ((d . d) (e . e) (f . f)))))
  '((cons x x)))

(record-bench 'eval-eval 'staged 'list-eval)
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
  '((list x (cons x '()) x)))

;; uses a metacirc eval w/map and list to synth the body of a procedure passed around in a higher order way and applied to several examples
(record-bench 'eval-eval 'unstaged 'list-eval #:description "Synthesis within a metacircular evaluator with list functions")
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
    (eval-and-map-and-list-evalo-unstaged `((lambda (proc)
                                     (list (map proc '())
                                           (map proc '(a))
                                           (map proc '(b c))
                                           (map proc '(d e f))))
                                   (lambda (x) ,q))
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

(test
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

(test
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

(record-bench 'eval-eval 'staging 'quasi-quine)
(defrel (quasi-quine-evalo expr val)
  (time-staged
   (evalo-staged (quasi-quine-eval expr) val)))

(record-bench 'eval-eval 'staged 'quasi-quine)
(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'struct q)
    (quasi-quine-evalo q q))
  `((((lambda (_.0) `(,_.0 ',_.0)) '(lambda (_.0) `(,_.0 ',_.0)))
     $$
     ,not-tags0+error
     (sym _.0))))

(record-bench 'eval-eval 'unstaged 'quasi-quine #:description "Synthesize a quine for a metacircular evaluator that adds \\texttt{quasiquote}")
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

;; Another way you can do quines a level up: the usual consy-quotey way. This test shows best indicates that what we're doing above w/qq is *not* this thing.
(defrel (ho-quine-interp-cons expr val)
  (staged
   (evalo-staged
    (ho-quine-interp-cons-fun `(eval-expr ',expr (lambda (y) 'error)))
    val)))

(test
  (run 1 (q)
    (absento 'error q)
    (absento 'struct q)
    (ho-quine-interp-cons q q))
  `((((lambda (_.0) (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0) (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     $$
     ,not-tags0+error
     (sym _.0))))

(test
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

(defrel (ho-double-evalo expr val)
  (staged
    (evalo-staged
     (ho-double-eval `(eval-expr ',expr (lambda (y) 'error)))
     val)))

(defrel (ho-double-evalo-unstaged expr val)
  (evalo-unstaged
   (ho-double-eval `(eval-expr ',expr (lambda (y) 'error)))
   val))

(test
  (run 1 (q) (ho-double-evalo '((lambda (x) x) 'hello) q))
  '(hello))

(test
  (run 1 (q) (ho-double-evalo-unstaged '((lambda (x) x) 'hello) q))
  '(hello))


(test
 (run 1 (q) (absento 'error q) (absento 'struct q) (ho-double-evalo q q))
 `((((lambda (_.0) (list _.0 (list 'quote _.0)))
    '(lambda (_.0) (list _.0 (list 'quote _.0))))
   $$
   ,not-tags0+error
   (sym _.0))))

(test
 (run 1 (q) (absento 'error q) (absento 'struct q) (ho-double-evalo-unstaged q q))
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

(defrel (map-in-double-eval expr val)
  (time-staged
    (evalo-staged
     (map-in-double-eval-fun `(eval-expr ',expr '()))
     val)))

(record-bench 'synth/ground-context 'staging 'map-eval)
(defrel (map-in-double-eval-used-for-synth expr val)
  (time-staged
    (evalo-staged
     (map-in-double-eval-fun `(eval-expr ',expr '()))
     val)))


(defrel (map-in-double-eval-unstaged expr val)
  (evalo-unstaged
   (map-in-double-eval-fun `(eval-expr ',expr '()))
   val))

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
    (map-in-double-eval-unstaged expr q)))
 '(((a . a) (b . b) (c . c))))

(record-bench 'synth/ground-context 'staged 'map-eval)
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
    (map-in-double-eval-used-for-synth expr '((a . a) (b . b) (c . c)))))
 '((cons x x)))

(record-bench 'synth/ground-context 'unstaged 'map-eval #:description "Body of function mapped via anonymous recursion in a metacircular evaluator")
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
    (map-in-double-eval-unstaged expr '((a . a) (b . b) (c . c)))))
 '((cons x x)))

(defrel (double-evalo expr val)
  (staged
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


(defrel (double-evalo-unstaged expr val)
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
   val))


(test
 (run 1 (q) (absento 'clo q) (double-evalo q q))
 `((((lambda (_.0) (list _.0 (list 'quote _.0)))
     '(lambda (_.0) (list _.0 (list 'quote _.0))))
    $$
    ,not-tags0+clo
    (sym _.0))))

(test
 (run 1 (q) (absento 'clo q) (double-evalo-unstaged q q))
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

(defrel (double-evalo-variadic-list-fo expr val)
  (staged
    (evalo-staged
     (double-evalo-variadic-list-fo-fun `(eval-expr ',expr '()))
     val)))

(defrel (double-evalo-variadic-list-fo-unstaged expr val)
  (evalo-unstaged
   (double-evalo-variadic-list-fo-fun `(eval-expr ',expr '()))
   val))


(test
  (run 1 (q) (absento 'clo q) (double-evalo-variadic-list-fo q q))
  `((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     $$
     ,not-tags0+clo
     (sym _.0))))

(test
  (run 1 (q) (absento 'clo q) (double-evalo-variadic-list-fo-unstaged q q))
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

(defrel (double-evalo-variadic-list-fo-less-ridiculous expr val)
  (staged
    (evalo-staged
     (double-evalo-variadic-list-fo-less-ridiculous-fun `(eval-expr ',expr '()))
     val)))

(defrel (double-evalo-variadic-list-fo-less-ridiculous-unstaged expr val)
  (evalo-unstaged
   (double-evalo-variadic-list-fo-less-ridiculous-fun `(eval-expr ',expr '()))
   val))

(test
  (run 1 (q) (absento 'clo q) (double-evalo-variadic-list-fo-less-ridiculous q q))
  `((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     $$
     ,not-tags0+clo
     (sym _.0))))

(test
  (run 1 (q) (absento 'clo q) (double-evalo-variadic-list-fo-less-ridiculous-unstaged q q))
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

(defrel (double-evalo-variadic-list-ho expr val)
  (staged
    (evalo-staged
     (double-evalo-variadic-list-ho-fun `(eval-expr ',expr '()))
     val)))

(defrel (double-evalo-variadic-list-ho-unstaged expr val)
  (evalo-unstaged
   (double-evalo-variadic-list-ho-fun `(eval-expr ',expr '()))
   val))


(test
  (run 1 (q) (absento 'clo q) (double-evalo-variadic-list-ho q q))
  `((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
   $$
   ,not-tags0+clo
   (sym _.0))))

(test
  (run 1 (q) (absento 'clo q) (double-evalo-variadic-list-ho-unstaged q q))
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

(defrel (double-evalo-cons expr val)
  (staged
    (evalo-staged
     (double-evalo-cons-fun `(eval-expr ',expr '()))
     val)))

(defrel (double-evalo-cons-unstaged expr val)
  (evalo-unstaged
   (double-evalo-cons-fun `(eval-expr ',expr '()))
   val))

(test
  (run 1 (q) (absento 'clo q) (double-evalo-cons q q))
  `((((lambda (_.0)
        (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0)
         (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     $$
     ,not-tags0+clo
     (sym _.0))))

(test
  (run 1 (q) (absento 'clo q) (double-evalo-cons-unstaged q q))
  `((((lambda (_.0)
        (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0)
         (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     $$
     ,not-tags0+clo
     (sym _.0))))
