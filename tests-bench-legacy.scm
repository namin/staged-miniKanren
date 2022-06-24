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

