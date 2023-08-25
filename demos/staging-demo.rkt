#lang racket

(require "../all.rkt")

(run* (q)
  (staged
   (later (== q 1))))

(pretty-write (generated-code))

(run* (q)
  (staged
   (fallback
    (later (== q 3))
    (conde
      ((== q 1) (later (== q 1)))
      ((== q 2) (later (== q 2)))))))

(run* (q)
  (staged
   (fresh ()
     (== q 1)
     (fallback
      (later (== q 3))
      (conde
        ((== q 1) (later (== q 1)))
        ((== q 2) (later (== q 2))))))))

(defrel (u-minio expr val)
  (conde
    ((numbero expr)
     (== expr val))
    ((symbolo expr)
     (== 'SYM val))
    ((fresh (e1 e2 v1 v2)
       (== `(,e1 ,e2) expr)
       (== `(,v1 ,v2) val)
       (u-minio e1 v1)
       (u-minio e2 v2)))))

(run* (q) (u-minio '(hello 1) q))
(run* (q) (u-minio q '(SYM 1)))

(defrel/staged (minio expr val)
  (fallback
   (later (u-minio expr val))
   (conde
     ((numbero expr) (later (== expr val)))
     ((symbolo expr) (later (== 'SYM val)))
     ((fresh (e f r v)
        (== `(,e ,f) expr)
        (later (== `(,r ,v) val))
        (minio e r)
        (minio f v))))))

(run* (q) (staged (minio '(hello 1) q)))
(pretty-write (generated-code))

(run* (q) (staged (minio q '(SYM 1))))
(pretty-write (generated-code))

(run 1 (q) (staged (fresh (a) (minio `(hello ,a) q))))
(pretty-write (generated-code))
