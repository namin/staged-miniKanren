(load "staged-load.scm")

(record-bench 'run-staged 'map-hole)
(time-test
 (run-staged 1 (q)
   (evalo-staged
    `(letrec ((map (lambda (f l)
                     (if (null? l)
                         '()
                         (cons (f (car l))
                               (map f (cdr l)))))))
       (map (lambda (x) ,q) '(a b c)))
    '((a . a) (b . b) (c . c))))
 '((cons x x)))

;; u-eval-expo seems 50% faster than the staged version
(record-bench 'unstaged 'map-hole)
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
   '((a (a) a) (b (b) b) (c (c) c))
   (lambda (q) (absento 'a q)))
 '((cons x (cons (cons x '()) (cons x '())))))

(time-test
  (run 1 (q)
    (absento 'a q)
    (u-eval-expo
     `(letrec ((map (lambda (f l)
                      (if (null? l)
                          '()
                          (cons (f (car l))
                                (map f (cdr l)))))))
        (map (lambda (x) ,q) '(a b c)))
     initial-env
     '((a (a) a) (b (b) b) (c (c) c))))
  '((cons x (cons (cons x '()) (cons x '())))))

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
 '(((lambda (_.0) (cons _.0 _.0))
    $$ (=/= ((_.0 call)) ((_.0 cons)) ((_.0 dynamic))) (sym _.0))))

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

;;;  I would have expected (car l) to be generated...
(time-test
 (run-staged 1 (q)
   (absento 'a q)
   (absento 'b q)
   (absento 'c q)
   (evalo-staged
     `(letrec ((map (lambda (f l)
                      (if (null? l)
                          '()
                          (cons (f ,q)
                                (map f (cdr l))))))
               (foo (lambda (f l1 l2)
                      (cons (map f l1)
                            (cons (map f l2)
                                  '())))))
        (foo (lambda (y) (cons y y)) '(a) '(b c)))
     '(((a . a)) ((b . b) (c . c)))))
  '(((match l
       (`(,_.0) _.0)
       (`(,_.1 unquote _.2) _.1)
       .
       _.3)
     $$
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
     $$
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
     $$
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
     $$
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
     $$
     (=/= ((_.0 a)) ((_.0 b)) ((_.0 c)) ((_.0 d)) ((_.0 e)) ((_.0 f)) ((_.1 _.2)) ((_.1 a)) ((_.1 b)) ((_.1 c)) ((_.1 d)) ((_.1 e)) ((_.1 f)) ((_.2 a)) ((_.2 b)) ((_.2 c)) ((_.2 d)) ((_.2 e)) ((_.2 f)))
     (sym _.0 _.1 _.2)
     (absento (a _.3) (b _.3) (c _.3) (d _.3) (e _.3) (f _.3)))))

;;  Tricky test, due to the nested map
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

;;  Runs faster than the staged version
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

#|
;;  I got bored of waiting for this test to return after ~1 minute.
;;  This is a very tough test, due to the nested `map`!
(time-test
  (syn-hole 1
            (lambda (q)
              `(letrec ((map (lambda (f l)
                                     (if (null? l)
                                         '()
                                         (cons (f ,q)
                                               (map f (cdr l)))))))
                 (map (lambda (l) (map (lambda (y) (cons y y)) l)) '((a) (b c) (d e f)))))
            '(((a . a))
              ((b . b) (c . c))
              ((d . d) (e . e) (f . f)))
            (lambda (q)
              (absento 'a q)
              (absento 'b q)
              (absento 'c q)
              (absento 'd q)
              (absento 'e q)
              (absento 'f q)))

  '(((match l
       (`(,_.0) _.0)
       (`(,_.1 . ,_.2) _.1)
       .
       _.3)
     $$
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
     $$
     (absento (a _.0) (b _.0) (c _.0)))))

(test
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
'(_.0))

(test
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
'(_.0))

(time-test
 (syn-hole 3
   (lambda (q)
     `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys
                      (cons (car xs) (append (cdr xs) ys))))))
        (append '(1 2) ,q)))
   '(1 2 3 4))
 '('(3 4) (list 3 4) ((letrec ((_.0 (lambda _.1 _.2))) '(3 4)) $$ (=/= ((_.0 quote))) (sym _.1))))

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
  (list 3 4)
  ((letrec ([_.0 (lambda _.1 _.2)]) '(3 4))
   $$
    (=/= ((_.0 quote)))
    (sym _.1))))

;;(record-bench 'run-staged 'appendo-tail)
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

;;(record-bench 'unstaged 'appendo-tail)
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

#|
(time-test
  (length
   (run 50 (q)
     (u-eval-expo
      q
      initial-env
      '(3 4))))
  50)
|#

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
   (list 1 2)
   ((letrec ([_.0 (lambda _.1 _.2)]) '(1 2))
    $$
    (=/= ((_.0 quote)))
    (sym _.1))))

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



(define-staged-relation (eval-and-map-evalo expr val)
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
   val))

(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'closure q)
    (eval-and-map-evalo `(map ,q '(a b c)) '((a . a) (b . b) (c . c))))
  '(((lambda (_.0) (cons _.0 _.0))
     $$
     (=/= ((_.0 call)) ((_.0 closure)) ((_.0 dynamic)) ((_.0 error)) ((_.0 prim)))
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
                        '(() ((a (a) a)) ((b (b) b) (c (c) c)) ((d (d) d) (e (e) e) (f (f) f)))))
  '((cons x (cons (cons x '()) (cons x '())))))

(record-bench 'unstaged 'eval-and-map-and-list-evalo)
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
    (u-eval-expo
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
     initial-env
     '(() ((a (a) a)) ((b (b) b) (c (c) c)) ((d (d) d) (e (e) e) (f (f) f)))))
  '((cons x (cons (cons x '()) (cons x '())))))

(record-bench 'staging 'eval-and-map-and-list-evalo)
(define eval-and-map-and-list-evalo
  (eval (time
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
                         [`(list . ,e*)
                          (map (lambda (e) (eval-expr e env)) e*)]
                         [`(,rator ,rand)
                          ((eval-expr rator env) (eval-expr rand env))]))])
             (eval-expr expr (lambda (y) 'error))))))))

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
    (absento 'closure q)
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
    (absento 'closure q)
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
    (absento 'closure q)
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

(define (quasi-quine-eval expr)
   `(letrec ([eval-quasi (lambda (q eval)
                           (match q
                             [(? symbol? x) x]
                             [`() '()]
                             [`(,`unquote ,exp) (eval exp)]
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
        (eval-expr ',expr (lambda (y) 'error)))))

(record-bench 'staging 'quasi-quine-evalo)
(define-staged-relation (quasi-quine-evalo expr val)
  (evalo-staged (quasi-quine-eval expr) val))

(record-bench 'staged 'quasi-quine-evalo)
(time-test
  (run 1 (q)
    (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
    (absento 'closure q)
    (quasi-quine-evalo q q))
  '((((lambda (_.0) `(,_.0 ',_.0)) '(lambda (_.0) `(,_.0 ',_.0)))
     $$
     (=/= ((_.0 call)) ((_.0 closure)) ((_.0 dynamic)) ((_.0 error)) ((_.0 prim)))
     (sym _.0))))

(record-bench 'run-staged 'quasi-quine-evalo)
(time-test
  (run-staged 1 (q)
    (absento 'error q)
    (absento 'closure q)
    (evalo-staged (quasi-quine-eval q) q))
  '((((lambda (_.0) `(,_.0 ',_.0)) '(lambda (_.0) `(,_.0 ',_.0)))
     $$
     (=/= ((_.0 call)) ((_.0 closure)) ((_.0 dynamic)) ((_.0 error)) ((_.0 prim)))
     (sym _.0))))

(record-bench 'unstaged 'quasi-quine-evalo)
(time-test
  (run 1 (q)
    (absento 'error q)
    (absento 'closure q)
    (evalo-unstaged (quasi-quine-eval q) q))
  '((((lambda (_.0) `(,_.0 ',_.0)) '(lambda (_.0) `(,_.0 ',_.0)))
     $$
     (=/= ((_.0 call)) ((_.0 closure)) ((_.0 dynamic)) ((_.0 error)) ((_.0 prim)))
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
     $$
     (=/= ((_.0 call)) ((_.0 closure)) ((_.0 dynamic)) ((_.0 error)) ((_.0 prim)))
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
    $$
    (=/= ((_.0 call)) ((_.0 closure)) ((_.0 dynamic)) ((_.0 prim)))
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

(record-bench 'staging 'proofo)
(define proofo
  (eval (time
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
           (proof? proof))))))

(define ex-proof1
  '((C (A (A => B) (B => C))
       modus-ponens
       (((B => C) (A (A => B) (B => C)) assumption ())
        (B (A (A => B) (B => C))
           modus-ponens
           (((A => B) (A (A => B) (B => C)) assumption ())
            (A (A (A => B) (B => C)) assumption ())))))))
(record-bench 'staged 'proofo 1)
(time-test
  (run 1 (prf)
    (fresh (body)
      (== prf `(C (A (A => B) (B => C)) . ,body))
      (proofo prf #t)))
  ex-proof1)

(define (prover prf)
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
       (proof? ',prf))))

(record-bench 'run-staged 'proofo 1)
(time-test
  (run-staged 1 (prf)
    (fresh (body)
      (== prf `(C (A (A => B) (B => C)) . ,body))
      (evalo-staged
       (prover prf)
       #t)))
  ex-proof1)

(record-bench 'unstaged 'proofo 1)
(time-test
  (run 1 (prf)
    (fresh (body)
      (== prf `(C (A (A => B) (B => C)) . ,body))
      (u-evalo
       (prover prf)
       #t)))
  ex-proof1)

(define ex-proof2
  '((((A => B) => ((B => C) => (A => C)))
     ()
     conditional
     ((((B => C) => (A => C))
       ((A => B))
       conditional
       (((A => C)
         ((B => C) (A => B))
         conditional
         ((C (A (B => C) (A => B))
             modus-ponens
             (((B => C) (A (B => C) (A => B)) assumption ())
              (B (A (B => C) (A => B))
                 modus-ponens
                 (((A => B) (A (B => C) (A => B)) assumption ())
                  (A (A (B => C) (A => B)) assumption ())))))))))))))

(record-bench 'staged 'proofo 2)
(time-test
  (run 1 (prf)
    (fresh (body)
      (== prf `(((A => B) => ((B => C) => (A => C))) () . ,body))
      (proofo prf #t)))
  ex-proof2)

(record-bench 'run-staged 'proofo 2)
(time-test
  (run-staged 1 (prf)
    (fresh (body)
      (== prf `(((A => B) => ((B => C) => (A => C))) () . ,body))
      (evalo-staged
       (prover prf)
       #t)))
  ex-proof2)

(record-bench 'unstaged 'proofo 2)
(time-test
  (run 1 (prf)
    (fresh (body)
      (== prf `(((A => B) => ((B => C) => (A => C))) () . ,body))
      (u-evalo
       (prover prf)
       #t)))
  ex-proof2)

(record-bench 'staged 'proofo 3)
(time-test
 (length
  (run 1 (prf)
    (fresh (body)
      (== prf `(((A => B) => ((B => C) => ((C => D)  => ((D => E) => (A => E))))) () . ,body))
      (proofo prf #t))))
  1)

(record-bench 'run-staged 'proofo 3)
(time-test
 (length
  (run-staged 1 (prf)
    (fresh (body)
      (== prf `(((A => B) => ((B => C) => ((C => D)  => ((D => E) => (A => E))))) () . ,body))
      (evalo-staged
       (prover prf)
       #t))))
 1)

#| doesn't come back
(record-bench 'unstaged 'proofo 3)
(time-test
 (length
  (run 1 (prf)
    (fresh (body)
      (== prf `(((A => B) => ((B => C) => ((C => D) ((D => E)  => (A => E))))) () . ,body))
      (u-evalo
       (prover prf)
       #t))))
  1)
|#

(record-bench 'staging 'double-evalo)
(define double-evalo
  (eval (time
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
           (eval-expr expr '()))))))

(record-bench 'staged 'double-evalo)
(time-test
 (run 1 (q) (absento 'clo q) (double-evalo q q))
 '((((lambda (_.0) (list _.0 (list 'quote _.0)))
     '(lambda (_.0) (list _.0 (list 'quote _.0))))
    $$
    (=/= ((_.0 call)) ((_.0 clo)) ((_.0 closure)) ((_.0 dynamic)) ((_.0 prim)))
    (sym _.0))))

(record-bench 'unstaged 'double-evalo)
(time-test
  (run 1 (q)
    (fresh (expr)
      (absento 'clo expr)
      (== q expr)
      (u-evalo
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
     $$
     (=/= ((_.0 call)) ((_.0 clo)) ((_.0 closure)) ((_.0 dynamic)) ((_.0 prim)))
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
     $$
     (=/= ((_.0 call)) ((_.0 clo)) ((_.0 closure)) ((_.0 dynamic)) ((_.0 prim)))
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
     $$
     (=/= ((_.0 call)) ((_.0 clo)) ((_.0 closure)) ((_.0 dynamic)) ((_.0 prim)))
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
   $$
   (=/= ((_.0 call)) ((_.0 clo)) ((_.0 closure)) ((_.0 dynamic)) ((_.0 prim)))
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
     $$
     (=/= ((_.0 call)) ((_.0 clo)) ((_.0 closure)) ((_.0 dynamic)) ((_.0 prim)))
     (sym _.0))))

