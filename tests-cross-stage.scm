(load "staged-load.scm")

(test
    (run-staged 1 (q)
      (evalo-staged '((lambda (x) x) 1) q))
  '(1))
