#lang racket/base

(require "../../main.rkt"
         "../../test-check.rkt")

(defrel/staged (unify5 x)
  (later (== x 5)))

(test
  (run* (q)
    (unify5 q))
  '(5))

(test
  (run* (q)
    (staged
     (unify5 q)))
  '(5))

(defrel/staged (minio expr val)
  (conde
    ((numbero expr) (later (== expr val)))
    ((symbolo expr) (later (== 'SYM val)))
    ((fresh (e f r v)
       (== `(,e ,f) expr)
       (later (== `(,r ,v) val))
       (minio e r)
       (minio f v)))))

(test
  (run* (q) (minio '(hello 1) q))
  '((SYM 1)))

(test
  (run* (q) (staged (minio '(hello 1) q)))
  '((SYM 1)))

(defrel/staged (minio-fallback expr val)
  (fallback
   (conde
     ((numbero expr) (later (== expr val)))
     ((symbolo expr) (later (== 'SYM val)))
     ((fresh (e f r v)
        (== `(,e ,f) expr)
        (later (== `(,r ,v) val))
       (minio-fallback e r)
        (minio-fallback f v))))))

(test
  (run* (q) (fresh (x)  (== x 1) (staged (minio-fallback `(hello ,x) q))))
  '((SYM 1)))

(test
  (run 1 (q) (fresh (x) (staged (minio-fallback `(hello ,x) q)) (== x 1)))
  '((SYM 1)))
