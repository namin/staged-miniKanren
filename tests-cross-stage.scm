(load "staged-load.scm")

(test
    (run-staged 1 (q)
      (evalo-staged '((lambda (x) x) 1) q))
  '(1))

(test
    (run-staged 1 (q)
      (evalo-staged '((lambda (x) ((lambda (y) y) x)) 1) q))
  '(1))

(todo ;; infinite loop?
    (run-staged 1 (q)
      (evalo-staged '(lambda (x) (((lambda (y) (lambda (z) z)) x) x)) q))
  '())
