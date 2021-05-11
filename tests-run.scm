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
                    (if (null? xs) ,q
                        (cons (car xs) (append (cdr xs) ys))))))
          (append '(1 2) '(3 4)))
       '(1 2 3 4)))
  '(ys))

(test
    (run-staged 1 (q)
      (evalo-staged `(,q (list 1 2)) 1)
      (l== q 'car))
  '(car))

(test
    (run-staged* (q)
      (l== q 'car)
      (evalo-staged `(,q (list 1 2)) 1))
  '(car))

(test
    (run-staged 1 (q)
      (fresh (q1 q2)
        (l== q `(,q1 ,q2))
        (evalo-staged `(,q1 (list ,q2 2)) q2)))
  '((not #f)))

(test
    (run-staged 1 (q1 q2)
      (evalo-staged `(,q1 (list ,q2 2)) q2))
  '((not #f)))

(define-relation (appendo xs ys zs)
  (conde
    ((== xs '()) (== ys zs))
    ((fresh (xa xd zd)
       (== (cons xa xd) xs)
       (== (cons xa zd) zs)
       (appendo xd ys zd)))))
(test
    (run* (xs ys)
      (appendo xs ys '(a b c)))
  '((() (a b c)) ((a) (b c)) ((a b) (c)) ((a b c) ())))

(define-staged-relation (test e) (staged-evalo `(cons ,e '()) '(5)))

(define-staged-relation (appendo xs ys zs)
  (evalo-staged
   `(letrec ((append
              (lambda (xs ys)
                (if (null? xs)
                    '()
                    (cons (car xs)
                          (append (cdr xs) ys))))))
      (append ',xs ',ys))
   zs))
(run* (q) (appendo q '(b) '(a b)))
