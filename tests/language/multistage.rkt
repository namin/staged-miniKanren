#lang racket/base

(require "../../main.rkt"
         "../../test-check.rkt")

(defrel/multistage/explicit (unify-mystery x)
  #:runtime
  (== x 5)
  #:staging-time
  (later (== x 6)))

(test
  (run* (q)
    (unify-mystery q))
  '(5))

(test
  (run* (q)
    (staged
     (unify-mystery q)))
  '(6))

(defrel/multistage (unify5 x)
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

(defrel/multistage (minio expr val)
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

#;
(defrel/multistage/explicit (minio-fallback expr val)
  #:runtime
  (minio expr val)
  #:staging-time
  (fallback
   (later (minio expr val))
   (minio expr val)))

(defrel/multistage/fallback (minio-fallback expr val)
  (conde
    ((numbero expr) (later (== expr val)))
    ((symbolo expr) (later (== 'SYM val)))
    ((fresh (e f r v)
       (== `(,e ,f) expr)
       (later (== `(,r ,v) val))
       (minio-fallback e r)
       (minio-fallback f v)))))

(test
  (run* (q) (fresh (x)  (== x 1) (staged (minio-fallback `(hello ,x) q))))
  '((SYM 1)))

(test
  (run 1 (q) (fresh (x) (staged (minio-fallback `(hello ,x) q)) (== x 1)))
  '((SYM 1)))
