(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(load "staged-interp.scm")
(load "staged-utils.scm")

(load "unstaged-interp.scm")

(load "test-check.scm")

(define (gen-hole query result)
  (let ((r (run 1 (q)
             (eval-expo #t
                        (query q)
                        initial-env
                        (quasi result)))))
    (let ((r (car r)))
      (fix-scope
       `(lambda (,(car r)) (fresh () . ,(caddr r)))))))
(define (syn-hole n query result)
  (let ((e (eval (gen-hole query result))))
    (run n (q) (e q))))



(time-test
 (syn-hole 1
   (lambda (q)
     `(letrec ((map (lambda (f l)
                      (if (null? l)
                          '()
                          (cons (f (car l))
                                (map f (cdr l)))))))
        (map (lambda (x) ,q) '(a b c))))
   '((a . a) (b . b) (c . c)))
 '((cons x x)))

;; u-eval-expo seems 50% faster than the syn-hole version
(time-test
  (run 1 (q)
    (u-eval-expo
     `(letrec ((map (lambda (f l)
                      (if (null? l)
                          '()
                          (cons (f (car l))
                                (map f (cdr l)))))))
        (map (lambda (x) ,q) '(a b c)))    
     initial-env
     '((a . a) (b . b) (c . c))))
  '((cons x x)))


(time-test
 (syn-hole 1
   (lambda (q)
     `(letrec ((map (lambda (f l)
                      (if (null? l)
                          '()
                          (cons (f (car l))
                                (map f (cdr l)))))))
        (map (lambda (x) ,q) '(a b c))))
   '((a (a) a) (b (b) b) (c (c) c)))
 '((((lambda _.0 _.0) x ((lambda _.1 _.1) x) x)
    (sym _.0 _.1))))

(time-test
  (run 1 (q)
    (u-eval-expo
     `(letrec ((map (lambda (f l)
                      (if (null? l)
                          '()
                          (cons (f (car l))
                                (map f (cdr l)))))))
        (map (lambda (x) ,q) '(a b c)))    
     initial-env
     '((a (a) a) (b (b) b) (c (c) c))))
  '((((lambda _.0 _.0) x ((lambda _.1 _.1) x) x)
    (sym _.0 _.1))))

#|
;;; Why is this version so slow?

(time-test
 (syn-hole 1
   (lambda (q)
     `(letrec ((map (lambda (f l)
                      (if (null? l)
                          '()
                          (cons (f (car l))
                                (map f (cdr l)))))))
        (map ,q '(a b c))))
   '((a . a) (b . b) (c . c)))
 '???)
|#

(time-test
  (syn-hole 1
    (lambda (q)
      `(letrec ((map (lambda (f l)
                       (if (null? l)
                           '()
                           (cons (f (car l))
                                 (map f (cdr l)))))))
         (map (lambda (y) y) '(d e))))
    '(d e))
  '(_.0))

;;; WEB I would have expected (car l) to be generated...
(time-test
  (let ((e (eval (gen-hole
                  (lambda (q)
                    `(letrec ((map (lambda (f l)
                                     (if (null? l)
                                         '()
                                         (cons (f ,q)
                                               (map f (cdr l))))))
                              (foo (lambda (f l1 l2)
                                     (cons (map f l1)
                                           (cons (map f l2)
                                                 '())))))
                       (foo (lambda (y) (cons y y)) '(a) '(b c))))
                  '(((a . a))
                    ((b . b) (c . c)))))))
    (run 1 (q)
      (absento 'a q)
      (absento 'b q)
      (absento 'c q)
      (e q)))
  '(((match l
       (`(,_.0) _.0)
       (`(,_.1 unquote _.2) _.1)
       .
       _.3)
     (=/= ((_.0 a)) ((_.0 b)) ((_.0 c)) ((_.1 _.2)) ((_.1 a)) ((_.1 b)) ((_.1 c)) ((_.2 a)) ((_.2 b)) ((_.2 c)))
     (sym _.0 _.1 _.2)
     (absento (a _.3) (b _.3) (c _.3)))))

(time-test
  (let ((e (eval (gen-hole
                  (lambda (q)
                    `(letrec ((map (lambda (f l)
                                     (if (null? l)
                                         '()
                                         (cons (f ,q)
                                               (map f (cdr l))))))
                              (foo (lambda (f l1 l2 l3)
                                     (cons (map f l1)
                                           (cons (map f l2)
                                                 (cons (map f l3)
                                                       '()))))))
                       (foo (lambda (y) (cons y y)) '(a) '(b c) '(d e f))))
                  '(((a . a))
                    ((b . b) (c . c))
                    ((d . d) (e . e) (f . f)))))))
    (run 1 (q)
      (absento 'a q)
      (absento 'b q)
      (absento 'c q)
      (absento 'd q)
      (absento 'e q)
      (absento 'f q)
      (e q)))
  '(((match l
       (`(,_.0) _.0)
       (`(,_.1 . ,_.2) _.1)
       .
       _.3)
     (=/= ((_.0 a)) ((_.0 b)) ((_.0 c)) ((_.0 d)) ((_.0 e)) ((_.0 f)) ((_.1 _.2)) ((_.1 a)) ((_.1 b)) ((_.1 c)) ((_.1 d)) ((_.1 e)) ((_.1 f)) ((_.2 a)) ((_.2 b)) ((_.2 c)) ((_.2 d)) ((_.2 e)) ((_.2 f)))
     (sym _.0 _.1 _.2)
     (absento (a _.3) (b _.3) (c _.3) (d _.3) (e _.3) (f _.3)))))

(time-test
  (let ((e (eval (gen-hole
                  (lambda (q)
                    `(letrec ((map (lambda (f l)
                                     (if (null? l)
                                         '()
                                         (cons (f ,q)
                                               (map f (cdr l))))))
                              (foo (lambda (f l1 l2 l3)
                                     ((lambda x x)
                                      (map f l1)
                                      (map f l2)
                                      (map f l3)))))
                       (foo (lambda (y) (cons y y)) '(a) '(b c) '(d e f))))
                  '(((a . a))
                    ((b . b) (c . c))
                    ((d . d) (e . e) (f . f)))))))
    (run 1 (q)
      (absento 'a q)
      (absento 'b q)
      (absento 'c q)
      (absento 'd q)
      (absento 'e q)
      (absento 'f q)
      (e q)))
  '(((match l
       (`(,_.0) _.0)
       (`(,_.1 . ,_.2) _.1)
       .
       _.3)
     (=/= ((_.0 a)) ((_.0 b)) ((_.0 c)) ((_.0 d)) ((_.0 e)) ((_.0 f)) ((_.1 _.2)) ((_.1 a)) ((_.1 b)) ((_.1 c)) ((_.1 d)) ((_.1 e)) ((_.1 f)) ((_.2 a)) ((_.2 b)) ((_.2 c)) ((_.2 d)) ((_.2 e)) ((_.2 f)))
     (sym _.0 _.1 _.2)
     (absento (a _.3) (b _.3) (c _.3) (d _.3) (e _.3) (f _.3)))))

(time-test
  (let ((e (eval (gen-hole
                  (lambda (q)
                    `(letrec ((map (lambda (f l)
                                     (if (null? l)
                                         '()
                                         (cons (f ,q)
                                               (map f (cdr l))))))
                              (foo (lambda (f lol)
                                     (if (null? lol)
                                         '()
                                         (cons (map f (car lol))
                                               (foo f (cdr lol)))))))
                       (foo (lambda (y) (cons y y)) '((a) (b c) (d e f)))))
                  '(((a . a))
                    ((b . b) (c . c))
                    ((d . d) (e . e) (f . f)))))))
    (run 1 (q)
      (absento 'a q)
      (absento 'b q)
      (absento 'c q)
      (absento 'd q)
      (absento 'e q)
      (absento 'f q)
      (e q)))
  '(((match l
       (`(,_.0) _.0)
       (`(,_.1 . ,_.2) _.1)
       .
       _.3)
     (=/= ((_.0 a)) ((_.0 b)) ((_.0 c)) ((_.0 d)) ((_.0 e)) ((_.0 f)) ((_.1 _.2)) ((_.1 a)) ((_.1 b)) ((_.1 c)) ((_.1 d)) ((_.1 e)) ((_.1 f)) ((_.2 a)) ((_.2 b)) ((_.2 c)) ((_.2 d)) ((_.2 e)) ((_.2 f)))
     (sym _.0 _.1 _.2)
     (absento (a _.3) (b _.3) (c _.3) (d _.3) (e _.3) (f _.3)))))

;; WEB Tricky test, due to the nested map
(time-test
  (let ((e (eval (gen-hole
                  (lambda (q)
                    `(letrec ((map (lambda (f l)
                                     (if (null? l)
                                         '()
                                         (cons (f (car ,q))
                                               (map f (cdr l)))))))
                       (map (lambda (l) (map (lambda (y) (cons y y)) l)) '((a) (b c) (d e f)))))
                  '(((a . a))
                    ((b . b) (c . c))
                    ((d . d) (e . e) (f . f)))))))
    (run 1 (q)
      (absento 'a q)
      (absento 'b q)
      (absento 'c q)
      (absento 'd q)
      (absento 'e q)
      (absento 'f q)
      (e q)))
  '(l))

;; WEB Runs faster than the staged version
(time-test
  (run 1 (q)
    (u-eval-expo
     `(letrec ((map (lambda (f l)
                      (if (null? l)
                          '()
                          (cons (f (car ,q))
                                (map f (cdr l)))))))
        (map (lambda (l) (map (lambda (y) (cons y y)) l)) '((a) (b c) (d e f))))    
     initial-env
     '(((a . a))
       ((b . b) (c . c))
       ((d . d) (e . e) (f . f)))))
  '(l))


(time-test
  (let ((e (eval (gen-hole
                  (lambda (q)
                    `(letrec ((map (lambda (f l)
                                     (if (null? l)
                                         '()
                                         (cons (f (car ,q))
                                               (map f (cdr l)))))))
                       (map (lambda (l) (map (lambda (y) (cons y y)) l)) '((a) (b c) (d e f)))))
                  '(((a . a))
                    ((b . b) (c . c))
                    ((d . d) (e . e) (f . f)))))))
    (run 1 (q)
      (absento 'a q)
      (absento 'b q)
      (absento 'c q)
      (absento 'd q)
      (absento 'e q)
      (absento 'f q)
      (e q)))
  '(l))

#|
;;; WEB Why does this fail??
(time-test
  (let ((e (eval (gen-hole
                  (lambda (q)
                    `(letrec ((map (lambda (f l)
                                     (if (null? l)
                                         '()
                                         (cons (f (,q l))
                                               (map f (cdr l)))))))
                       (map (lambda (l) (map (lambda (y) (cons y y)) l)) '((a) (b c) (d e f)))))
                  '(((a . a))
                    ((b . b) (c . c))
                    ((d . d) (e . e) (f . f)))))))
    (run 1 (q)
      (absento 'a q)
      (absento 'b q)
      (absento 'c q)
      (absento 'd q)
      (absento 'e q)
      (absento 'f q)
      (e q)))
  '(car))
|#

#|
;; WEB I got bored of waiting for this test to return after ~1 minute.
;; WEB This is a very tough test, due to the nested `map`!
(time-test
  (let ((e (eval (gen-hole
                  (lambda (q)
                    `(letrec ((map (lambda (f l)
                                     (if (null? l)
                                         '()
                                         (cons (f ,q)
                                               (map f (cdr l)))))))
                       (map (lambda (l) (map (lambda (y) (cons y y)) l)) '((a) (b c) (d e f)))))
                  '(((a . a))
                    ((b . b) (c . c))
                    ((d . d) (e . e) (f . f)))))))
    (run 1 (q)
      (absento 'a q)
      (absento 'b q)
      (absento 'c q)
      (absento 'd q)
      (absento 'e q)
      (absento 'f q)
      (e q)))
  '(((match l
       (`(,_.0) _.0)
       (`(,_.1 . ,_.2) _.1)
       .
       _.3)
     (=/= ((_.0 a)) ((_.0 b)) ((_.0 c)) ((_.0 d)) ((_.0 e)) ((_.0 f)) ((_.1 _.2)) ((_.1 a)) ((_.1 b)) ((_.1 c)) ((_.1 d)) ((_.1 e)) ((_.1 f)) ((_.2 a)) ((_.2 b)) ((_.2 c)) ((_.2 d)) ((_.2 e)) ((_.2 f)))
     (sym _.0 _.1 _.2)
     (absento (a _.3) (b _.3) (c _.3) (d _.3) (e _.3) (f _.3)))))
|#
(time-test
  (let ((e (eval (gen-hole
                  (lambda (q)
                    `(letrec ((map (lambda (f l)
                                     (if (null? l)
                                         '()
                                         (cons (f (car l))
                                               (map f (cdr l)))))))
                       (map (lambda (y) (cons y y)) '(a b c))))
                  '((a . a) (b . b) (c . c))))))
    (run 1 (q)
      (absento 'a q)
      (absento 'b q)
      (absento 'c q)
      (e q)))
  '((_.0
     (absento (a _.0) (b _.0) (c _.0)))))

#|
;; WEB Why does this test fail?
(syn-hole 1
  (lambda (q)
    `(letrec ((map (lambda (f l)
                     (if (null? l)
                         '()
                         (cons (f (car l))
                               (map f (cdr l)))))))
       (cons (map (lambda (y) y) '(d e))
             '())))
  '((d e)))
|#

#|
;; WEB Why does this test fail?
(syn-hole 1
  (lambda (q)
    `(letrec ((map (lambda (f l)
                     (if (null? l)
                         '()
                         (cons (f (car l))
                               (map f (cdr l)))))))
       ((lambda x x)
        (map (lambda (y) y) '(d e))
        (map (lambda (x) (cons x x)) '(a b c)))))
  '((d e)
    ((a . a) (b . b) (c . c))))
|#

(time-test
 (syn-hole 3
   (lambda (q)
     `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys
                      (cons (car xs) (append (cdr xs) ys))))))
        (append '(1 2) ,q)))
   '(1 2 3 4))
 '('(3 4)
   (((lambda _.0 _.0) 3 4) (sym _.0))
   (((lambda _.0 '(3 4))) (=/= ((_.0 quote))) (sym _.0))))

(time-test
 (run 3 (q)
   (u-eval-expo
    `(letrec ((append
               (lambda (xs ys)
                 (if (null? xs) ys
                     (cons (car xs) (append (cdr xs) ys))))))
       (append '(1 2) ,q))    
    initial-env
    '(1 2 3 4)))
 '('(3 4)
   (((lambda _.0 _.0) 3 4) (sym _.0))
   (((lambda _.0 '(3 4))) (=/= ((_.0 quote))) (sym _.0))))


(time-test
 (length
  (syn-hole 50
   (lambda (q)
     `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys
                      (cons (car xs) (append (cdr xs) ys))))))
        (append '(1 2) ,q)))
   '(1 2 3 4)))
 50)

(time-test
  (length
   (run 50 (q)
     (u-eval-expo
      `(letrec ((append
                 (lambda (xs ys)
                   (if (null? xs) ys
                       (cons (car xs) (append (cdr xs) ys))))))
         (append '(1 2) ,q))    
      initial-env
      '(1 2 3 4))))
  50)

(time-test
  (length
   (run 50 (q)
     (u-eval-expo
      q    
      initial-env
      '(3 4))))
  50)


(time-test
 (syn-hole 3
   (lambda (q)
     `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys
                      (cons (car xs) (append (cdr xs) ys))))))
        (append ,q '(3 4))))
   '(1 2 3 4))
 '('(1 2)
   (((lambda _.0 _.0) 1 2) (sym _.0))
   (((lambda _.0 '(1 2))) (=/= ((_.0 quote))) (sym _.0))))

(time
 (run 3 (q)
   (u-eval-expo
    `(letrec ((append
               (lambda (xs ys)
                 (if (null? xs) ys
                     (cons (car xs) (append (cdr xs) ys))))))
       (append ,q '(3 4)))    
    initial-env
    '(1 2 3 4))))



(define eval-and-map-evalo
  (eval
   (gen 'eval-expr '(expr)
        `(letrec ([map (lambda (f l)
                         (if (null? l)
                             '()
                             (cons (f (car l))
                                   (map f (cdr l)))))]
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
           (eval-expr expr (lambda (y) 'error))))))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'closure q)
    (eval-and-map-evalo `(map ,q '(a b c)) '((a . a) (b . b) (c . c))))
  '(((lambda (_.0) (cons _.0 _.0))
     (=/= ((_.0 closure)) ((_.0 error)))
     (sym _.0))))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'closure q)
    (eval-and-map-evalo `(map (lambda (x) ,q) '(a b c)) '((a . a) (b . b) (c . c))))
  '((cons x x)))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'closure q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (eval-and-map-evalo `(map (lambda (x) ,q) '(a b c)) '((a . a) (b . b) (c . c))))
  '((cons x x)))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'closure q)
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
    (absento 'closure q)
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
    (absento 'closure q)
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



(define quasi-quine-evalo
  (eval
   (gen 'eval-expr '(expr)
        `(letrec ([eval-quasi (lambda (q eval)
                                (match q
                                  [(? symbol? x) x]
                                  [`() '()]
                                  [`(,`unquote ,exp) (eval exp)]
                                  [`(quasiquote ,datum) 'error] ;; was ('error) in the 2017 ICFP Pearl, but
                                  ;; the code generator rejects this erroneous code!
                                  [`(,a . ,d)
                                   (cons (eval-quasi a eval) (eval-quasi d eval))]))]
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
                        ((eval-expr rator env) (eval-expr rand env))]))])
           (eval-expr expr (lambda (y) 'error))))))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'closure q)
    (quasi-quine-evalo q q))
  '((((lambda (_.0) `(,_.0 ',_.0)) '(lambda (_.0) `(,_.0 ',_.0)))
     (=/= ((_.0 closure)) ((_.0 error)))
     (sym _.0))))


(define ho-quine-interp-cons
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
    (absento 'closure q)
    (ho-quine-interp-cons q q))
  '((((lambda (_.0) (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0) (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     (=/= ((_.0 closure)) ((_.0 error)))
     (sym _.0))))


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

(time-test
 (run 2 (q) (absento 'closure q) (ho-double-evalo q q))
 '(error
   (((lambda (_.0) (list _.0 (list 'quote _.0)))
     '(lambda (_.0) (list _.0 (list 'quote _.0))))
    (=/= ((_.0 closure)))
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
