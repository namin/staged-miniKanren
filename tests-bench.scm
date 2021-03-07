;; using namin/faster-miniKaren branch staged
(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(load "staged-interp.scm")
(load "staged-utils.scm")

(load "test-check.scm")


(define quasi-quine-evalo-single-letrec
  (eval
   (gen 'eval-expr '(expr)
        `(letrec [(eval-quasi (lambda (q eval)
                                (match q
                                  [(? symbol? x) x]
                                  [`() '()]
                                  [`(,`unquote ,exp) (eval exp)]
                                  [`(quasiquote ,datum) ('error)]
                                  [`(,a . ,d)
                                   (cons (eval-quasi a eval) (eval-quasi d eval))])))]
                 [eval-expr
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
                       ((eval-expr rator env) (eval-expr rand env))]))]
                 (eval-expr ',q
                            'initial-env)))))

(time-test
  (run 1 (q) (quasi-quine-evalo-single-letrec q q))
  '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))



(define quasi-quine-evalo
  (eval
   (gen 'eval-expr '(expr)
        `(letrec ((eval-quasi (lambda (q eval)
                                (match q
                                  [(? symbol? x) x]
                                  [`() '()]
                                  [`(,`unquote ,exp) (eval exp)]
                                  [`(quasiquote ,datum) ('error)]
                                  [`(,a . ,d)
                                   (cons (eval-quasi a eval) (eval-quasi d eval))]))))
           (letrec ((eval-expr
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
                          ((eval-expr rator env) (eval-expr rand env))]
                         ))))
             (eval-expr ',q
                        'initial-env))))))

(time-test
  (run 1 (q) (quasi-quine-evalo q q))
  '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))


(define ho-double-evalo
  (eval
   (gen 'eval-expr '(expr)
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
           (eval-expr expr (lambda (y) 'error))))))

(time-test
  (run 1 (q) (ho-double-evalo '((lambda (x) x) 'hello) q))
  '(hello))

#|
(load "unstaged-interp.scm")
(time-test
 (run 2 (q) (absento 'closure q) (ho-double-evalo q q))
 '(error
   (((lambda (_.0) (list _.0 (list 'quote _.0)))
     '(lambda (_.0) (list _.0 (list 'quote _.0))))
    (=/= ((_.0 closure)))
    (sym _.0))))

|#

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

(define map-in-double-eval
  (eval
   (gen 'eval-expr '(expr)
        `(letrec ([lookup
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
           (eval-expr expr '())))))

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

(define proofo
  (eval
   (gen 'proof? '(proof)
        '(letrec ([member?
                   (lambda (x ls)
                     (if (null? ls) #f
                         (if (equal? (car ls) x) #t
                             (member? x (cdr ls)))))]
                  [proof?
                   (lambda (proof)
                     (match proof
                       [`(,A ,assms assumption ()) (member? A assms)]
                       [`(,B ,assms modus-ponens
                             (((,A => ,B) ,assms ,r1 ,ants1)
                              (,A ,assms ,r2 ,ants2)))
                        (and (proof? (list (list A '=> B) assms r1 ants1))
                             (proof? (list A assms r2 ants2)))]
                       [`((,A => ,B) ,assms conditional
                          ((,B (,A . ,assms) ,rule ,ants)))
                        (proof? (list B (cons A assms) rule ants))]))])
           (proof? proof)))))

(time-test
  (run 1 (prf)
    (fresh (body)
      (== prf `(C (A (A => B) (B => C)) . ,body))
      (proofo prf #t)))
  '((C (A (A => B) (B => C))
       modus-ponens
       (((B => C) (A (A => B) (B => C)) assumption ())
        (B (A (A => B) (B => C))
           modus-ponens
           (((A => B) (A (A => B) (B => C)) assumption ())
            (A (A (A => B) (B => C)) assumption ())))))))



(define double-evalo
  (eval
   (gen 'eval-expr '(expr)
        `(letrec ([lookup
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
                       [`(lambda (,(? symbol? x)) ,body)
                        (list 'clo x body env)]
                       [`(list ,e1 ,e2)
                        (list (eval-expr e1 env) (eval-expr e2 env))]
                       [(? symbol? x) (lookup x env)]
                       [`(,rator ,rand)
                        (match (eval-expr rator env)
                          [`(clo ,x ,body ,clo-env)
                           (eval-expr body (cons (cons x (eval-expr rand env)) clo-env))])]))])
           (eval-expr expr '())))))

(time-test
 (run 1 (q) (absento 'clo q) (double-evalo q q))
 '((((lambda (_.0) (list _.0 (list 'quote _.0)))
    '(lambda (_.0) (list _.0 (list 'quote _.0))))
    (=/= ((_.0 clo)) ((_.0 closure)))
    (sym _.0))))


(define double-evalo-variadic-list-fo
  (eval
   (gen 'eval-expr '(expr)
        `(letrec ([lookup
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
           (eval-expr expr '())))))

(time-test
  (run 1 (q) (absento 'clo q) (double-evalo-variadic-list-fo q q))
  '((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 clo)) ((_.0 closure)))
     (sym _.0))))


(define double-evalo-variadic-list-fo-less-ridiculous
  (eval
   (gen 'eval-expr '(expr)
        `(letrec ([lookup
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
           (eval-expr expr '())))))

(time-test
  (run 1 (q) (absento 'clo q) (double-evalo-variadic-list-fo-less-ridiculous q q))
  '((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 clo)) ((_.0 closure)))
     (sym _.0))))


(define double-evalo-variadic-list-ho
  (eval
   (gen 'eval-expr '(expr)
        `(letrec ([lookup
                   (lambda (x env)
                     (match env
                       [`((,y . ,v) . ,renv)
                        (if (equal? x y)
                            v
                            (lookup x renv))]))]
                  [map
                   (lambda (f l)
                     (match l
                       [`() '()]
                       [`(,a . ,d)
                        (cons (f a) (map f d))]))]
                  [eval-expr
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
           (eval-expr expr '())))))

(time-test
  (run 1 (q) (absento 'clo q) (double-evalo-variadic-list-ho q q))
  '((((lambda (_.0) (list _.0 (list 'quote _.0)))
    '(lambda (_.0) (list _.0 (list 'quote _.0))))
   (=/= ((_.0 clo)) ((_.0 closure)))
   (sym _.0))))

(define double-evalo-cons
  (eval
   (gen 'eval-expr '(expr)
        `(letrec ([lookup
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
                       [`(lambda (,(? symbol? x)) ,body)
                        (list 'clo x body env)]
                       [`(cons ,e1 ,e2)
                        (cons (eval-expr e1 env) (eval-expr e2 env))]
                       [(? symbol? x) (lookup x env)]
                       [`(,rator ,rand)
                        (match (eval-expr rator env)
                          [`(clo ,x ,body ,clo-env)
                           (eval-expr body (cons (cons x (eval-expr rand env)) clo-env))])]))])
           (eval-expr expr '())))))

(time-test
  (run 1 (q) (absento 'clo q) (double-evalo-cons q q))
  '((((lambda (_.0)
        (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0)
         (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     (=/= ((_.0 clo)) ((_.0 closure)))
     (sym _.0))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#!eof


(format #t "\n\n -- UNSTAGED --\n\n")
(load "full-interp.scm")

(time-test
  (run 1 (prf)
    (fresh (body)
      (== prf `(C (A (A => B) (B => C)) . ,body))
      (evalo
       `(letrec ([member?
                  (lambda (x ls)
                    (if (null? ls) #f
                        (if (equal? (car ls) x) #t
                            (member? x (cdr ls)))))])
          (letrec ([proof?
                    (lambda (proof)
                      (match proof
                        [`(,A ,assms assumption ()) (member? A assms)]
                        [`(,B ,assms modus-ponens
                              (((,A => ,B) ,assms ,r1 ,ants1)
                               (,A ,assms ,r2 ,ants2)))
                         (and (proof? (list (list A '=> B) assms r1 ants1))
                              (proof? (list A assms r2 ants2)))]
                        [`((,A => ,B) ,assms conditional
                           ((,B (,A . ,assms) ,rule ,ants)))
                         (proof? (list B (cons A assms) rule ants))]
                        ))])
            (proof? ',prf)))
       #t)))
  '((C (A (A => B) (B => C))
       modus-ponens
       (((B => C) (A (A => B) (B => C)) assumption ())
        (B (A (A => B) (B => C))
           modus-ponens
           (((A => B) (A (A => B) (B => C)) assumption ())
            (A (A (A => B) (B => C)) assumption ())))))))


(time-test
  (run 1 (q)
    (fresh (expr)
      (absento 'clo expr)
      (== q expr)
      (evalo
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
  '((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 clo)) ((_.0 closure)) ((_.0 prim)))
     (sym _.0))))
