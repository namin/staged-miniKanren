(run* (q)
  (l== q 1))

(run-staged* (q)
  (l== q 1))
res

(run-staged* (q)
  (conde
    ((l== q 1))
    ((l== q 2))))

(run-staged* (q)
  (condg
   (l== q 3)
   ([] [(== q 1)] [(l== q 1)])
   ([] [(== q 2)] [(l== q 2)])))

(run-staged* (q)
  (== q 1)
  (condg
   (l== q 3)
   ([] [(== q 1)] [(l== q 1)])
   ([] [(== q 2)] [(l== q 2)])))

(define (u-minio expr val)
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

(define (minio expr val)
  (condg
   (u-minio expr val)
   ([] [(numbero expr)] [(l== expr val)])
   ([] [(symbolo expr)] [(l== 'SYM val)])
   ([e1 e2 v1 v2] [(== `(,e1 ,e2) expr)]
    [(l== `(,v1 ,v2) val)
     (minio e1 v1)
     (minio e2 v2)])))

(run-staged* (q) (minio '(hello 1) q))
res

(run 10 (q) (fresh (a) (minio `(hello ,a) q)))
(run-staged* (q) (fresh (a) (minio `(hello ,a) q)))
