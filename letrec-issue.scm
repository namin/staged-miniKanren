(test
    (run 1 (q)
      (fresh (v)
        (u-eval-expo
         '(f 1)
         `((f . (val . ,v)) . ,initial-env)
         1)
        (u-eval-expo
         `(lambda (x) ,q)
         initial-env
         v)))
  '(1))

(test
    (run-staged 1 (q)
      (fresh (v)
        (lift `(u-eval-expo
                '(f 1)
                `((f . (val . ,,(expand v))) . ,initial-env)
                1))
        (eval-expo
         #t
         `(lambda (x) ,q)
         initial-env
         v)))
  '(1))

(test
    (run 1 (q)
      (fresh (v)
        (u-eval-expo
         '(f 1)
         `((f . (val . ,v)) . ,initial-env)
         1)
        (u-eval-expo
         `(letrec ([g (lambda (x) ,q)]) g)
         initial-env
         v)))
  '(1))

#|
(run-staged 1 (q)
  (fresh (v)
    (lift `(u-eval-expo
             '(f 1)
             `((f . (val . ,,(expand v))) . ,initial-env)
             1))
    (eval-expo
      #t
      `(letrec ([g (lambda (x) ,q)]) g)
      initial-env
      v)))

divergence
This happens because the closure created by the first eval running backwards cannot unify with the representation of letrec-defined functions from the staged interpreter.
|#

;; this works
(run-staged 1 (q)
  (evalo-staged
   `(letrec ((id (lambda (x) x)))
      ((lambda (f) ,q) id))
   1)
  ;; strange when commenting this out
  ;;(l== q `(f 1))
  )

;; letrec mutual recursion not compatible with env lookup in unstaged interp.
(run 1 (q)
  (evalo-staged
   `(letrec ((f (lambda (x) ,q))
             (g (lambda (x) x)))
      1)
   1))
