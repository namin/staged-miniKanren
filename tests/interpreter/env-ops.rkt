#lang racket/base

(require "../../all.rkt"
         (submod "../../staged-interp.scm" private))

(test
 (run* (x v)
   (lookupo x initial-env v))
 '((list (struct prim . list))
   (not (struct prim . not))
   (equal? (struct prim . equal?))
   (symbol? (struct prim . symbol?))
   (number? (struct prim . number?))
   (pair? (struct prim . pair?))
   (cons (struct prim . cons))
   (null? (struct prim . null?))
   (car (struct prim . car))
   (cdr (struct prim . cdr))))

(test
 (run* (v1 v2)
   (fresh (env)
     (ext-env*o '(x y) '(1 2) empty-env env)
     (lookupo 'x env v1)
     (lookupo 'y env v2)))
 '((1 2)))

(test
 (run* (q)
   (fresh (env)
     (ext-env*o '(x y) '(1 2) empty-env env)
     (not-in-envo 'x env)))
 '())

(test
 (run* (q)
   (fresh (env)
     (ext-env*o '(x y) '(1 2) empty-env env)
     (not-in-envo 'z env)))
 '(_.0))

(test
 (run* (v1 v2 v3 v4)
   (fresh (env1 env2 env)
     (ext-env*o '(x y) '(1 2) empty-env env1)
     (ext-env*o '(a b) '(3 4) empty-env env2)
     (regular-env-appendo env1 env2 env)
     (lookupo 'x env v1)
     (lookupo 'y env v2)
     (lookupo 'a env v3)
     (lookupo 'b env v4)))
 '((1 2 3 4)))

(test
 (run* (v1 v2 v3)
   (fresh (env1 env2 env)
     (ext-env*o '(x y) '(1 2) empty-env env1)
     (ext-env*o '(a x) '(3 4) empty-env env2)
     (regular-env-appendo env1 env2 env)
     (lookupo 'x env v1)
     (lookupo 'y env v2)
     (lookupo 'a env v3)))
 '((1 2 3)))
