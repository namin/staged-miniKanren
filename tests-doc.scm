(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(load "staged-interp.scm")
(load "unstaged-interp.scm")
(load "staged-utils.scm")
(load "staged-run.scm")

(load "test-check.scm")

(define evalo evalo-unstaged)

;; # Background

;; ## miniKanren

(test
    (run* (q) (== q 5))
  '(5))

(test
    (run* (q) (== 5 6))
  '())

(test
    (run* (q) (== 5 5))
  '(_.0))

(test
    (run* (q) (fresh (x y) (== q `(,x ,y))))
  '((_.0 _.1)))

(test
    (run* (q) (fresh (x y) (== q `(,x ,y)) (== x 5)))
  '((5 _.0)))

(test
    (run* (q)
      (conde
        ((== q 5))
        ((== q 6))))
  '(5 6))

(test
    (run 1 (q)
      (conde
        ((== q 5))
        ((== q 6))))
  '(5))

(define (appendo xs ys zs)
  (conde
    ((== xs '()) (== ys zs))
    ((fresh (xa xd zd)
       (== xs (cons xa xd))
       (== zs (cons xa zd))
       (appendo xd ys zd)))))

(test
    (run* (q) (appendo '(a b) '(c d e) q))
  '((a b c d e)))

(test
    (run* (q) (fresh (x y) (== q (list x y)) (appendo x y '(a b c d e))))
  '(
    (() (a b c d e))
    ((a) (b c d e))
    ((a b) (c d e))
    ((a b c) (d e))
    ((a b c d) (e))
    ((a b c d e) ())
    ))

(test
    (run* (x y z) (appendo `(a  . ,x) `(,y e) `(a b c d ,z)))
  '(((b c) d e)))

;; ## Relational Interpreters

(test
    (run* (q) (evalo 1 q))
  '(1))

(test
    (run* (q) (evalo (car '(1 2)) q))
  '(1))

(todo "too slow in this version of evalo"
    (run 4 (q) (evalo q q))
  '((_.0 (num _.0))
   #t
   #f
   (((lambda (_.0) (list _.0 (list 'quote _.0)))
     '(lambda (_.0) (list _.0 (list 'quote _.0))))
    (=/= ((_.0 closure))
         ((_.0 list))
         ((_.0 prim))
         ((_.0 quote)))
    (sym _.0))))

(test
    ((lambda (x) (list x (list 'quote x)))
     '(lambda (x) (list x (list 'quote x))))
  '((lambda (x) (list x (list 'quote x)))
    '(lambda (x) (list x (list 'quote x)))))

(test
    (run* (x y)
      (evalo
       `(letrec ((append (lambda (xs ys)
                           (if (null? xs) ys
                               (cons (car xs) (append (cdr xs) ys))))))
          (append ',x ',y))
       '(a b c d e)))
  '(
    (() (a b c d e))
    ((a) (b c d e))
    ((a b) (c d e))
    ((a b c) (d e))
    ((a b c d) (e))
    ((a b c d e) ())
    ))

;; REST

(define-staged-relation (appendo xs ys zs)
  (evalo-staged
   `(letrec ((append
              (lambda (xs ys)
                (if (null? xs)
                    ys
                    (cons (car xs)
                          (append (cdr xs) ys))))))
      (append ',xs ',ys))
   zs))

(test
    (run* (q) (appendo '(a b) '(c d e) q))
    '((a b c d e)))

(test
    (run* (x y) (appendo x y '(a b c d e)))
  '(
    (() (a b c d e))
    ((a) (b c d e))
    ((a b) (c d e))
    ((a b c) (d e))
    ((a b c d) (e))
    ((a b c d e) ())
    ))

(test
    (run-staged 1 (q)
      (evalo-staged
       `(letrec ((append
                  (lambda (xs ys)
                    (if (null? xs) ys
                        (cons (car xs) (append (cdr xs) ys))))))
          (append '(1 2) '(3 4)))
       q))
  '((1 2 3 4)))

(test
    (run 1 (q)
      (evalo-unstaged
       `(letrec ((append
                  (lambda (xs ys)
                    (if (null? xs) ys
                        (cons (car xs) (append (cdr xs) ys))))))
          (append '(1 2) '(3 4)))
       q))
  '((1 2 3 4)))


(test
    (run-staged 5 (q)
      (evalo-staged
       q
       '(I love staged evaluation)))
  '('(I love staged evaluation)
    (((lambda _.0 '(I love staged evaluation))) $$ (=/= ((_.0 quote))) (sym _.0))
    ((car '((I love staged evaluation) . _.0)) $$ (absento (call _.0) (closure _.0) (dynamic _.0) (prim _.0)))
    (cons 'I '(love staged evaluation))
    (((lambda _.0 '(I love staged evaluation)) _.1) $$ (=/= ((_.0 quote))) (num _.1) (sym _.0))))

(test
    (run 5 (q)
      (evalo-unstaged
       q
       '(I love staged evaluation)))
  '('(I love staged evaluation)
    (((lambda _.0 '(I love staged evaluation))) $$ (=/= ((_.0 quote))) (sym _.0))
    ((car '((I love staged evaluation) . _.0)) $$ (absento (call _.0) (closure _.0) (dynamic _.0) (prim _.0)))
    (cons 'I '(love staged evaluation))
    (((lambda _.0 '(I love staged evaluation)) _.1) $$ (=/= ((_.0 quote))) (num _.1) (sym _.0))))

