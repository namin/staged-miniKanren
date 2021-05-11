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
