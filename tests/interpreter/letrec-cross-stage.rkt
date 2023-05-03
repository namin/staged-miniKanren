#lang racket/base

(require "../../all.rkt")

;; unstaged call before unstaged lambda
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

;; unstaged call before staged lambda
(test
 (run 1 (q)
      (staged
       (fresh (v)
         (later (u-eval-expo
                 '(f 1)
                 `((f . (val . ,v)) . ,initial-env)
                 1))
         (eval-expo
          `(lambda (x) ,q)
          initial-env
          v))))
 '(1))

;; unstaged call before unstaged letrec
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

;; unstaged call before staged letrec
(test
 (run 1 (q)
      (staged
       (fresh (v)
         (later (u-eval-expo
                 '(f 1)
                 `((f . (val . ,v)) . ,initial-env)
                 1))
         (eval-expo
          `(letrec ([g (lambda (x) ,q)]) g)
          initial-env
          v))))
 '(1))


;; staged interpreter's letrec closures work in unstaged fallback
(test
 (run 1 (q)
      (staged
       (evalo-staged
        `(letrec ((id (lambda (x) x)))
           ((lambda (f) ,q) id))
        1))
      (== q `(f 1)))
 '((f 1)))

;; the interpreter currently only supports single-bindings letrec
#;(run 1 (q)
     (staged
      (evalo-staged
       `(letrec ((f (lambda (x) ,q))
                 (g (lambda (x) x)))
          1)
       1)))
