(load "staged-load.scm")

(test
    (run-staged 1 (q)
      (evalo-staged
       `(letrec^ ((f (lambda (x) x))) 1)
       q))
  '(1))

(test
    (run-staged 1 (q)
      (evalo-staged
       `(letrec^ ((f (lambda (x) x))) (f 1))
       q))
  '(1))
