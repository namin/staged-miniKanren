(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(load "staged-interp.scm")
(load "unstaged-interp.scm")
(load "staged-utils.scm")
(load "staged-run.scm")

(load "test-check.scm")

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
       
       q))
  '('(I love staged evaluation)
    (((lambda _.0 '(I love staged evaluation))) $$ (=/= ((_.0 quote))) (sym _.0))
    ((car '((I love staged evaluation) . _.0)) $$ (absento (call _.0) (closure _.0) (dynamic _.0) (prim _.0)))
    (cons 'I '(love staged evaluation))
    (((lambda _.0 '(I love staged evaluation)) _.1) $$ (=/= ((_.0 quote))) (num _.1) (sym _.0))))

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
    (run* (x y z) (appendo `(a  . ,x) `(,y e) `(a b c d ,z)))
  '(((b c) d e)))
