(run* (q)
  (staged
   (later (== q 1))))
(generated-code)

(run* (q)
  (staged
   (condg
    #:fallback (later (== q 3))
    ([] [(== q 1)] [(later (== q 1))])
    ([] [(== q 2)] [(later (== q 2))]))))

(run* (q)
  (staged
   (fresh ()
     (== q 1)
     (condg
      #:fallback (later (== q 3))
      ([] [(== q 1)] [(later (== q 1))])
      ([] [(== q 2)] [(later (== q 2))])))))

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

(defrel (minio expr val)
  (staged
   (condg
    #:fallback (later (u-minio expr val))
    ([] [(numbero expr)] [(later (== expr val))])
    ([] [(symbolo expr)] [(later (== 'SYM val))])
    ([e1 e2 v1 v2] [(== `(,e1 ,e2) expr)]
     [(later (== `(,v1 ,v2) val))
      (minio e1 v1)
      (minio e2 v2)]))))

(run* (q) (staged (minio '(hello 1) q)))
(generated-code)

(run* (q) (staged (minio q '(SYM 1))))
(generated-code)

(run 1 (q) (staged (fresh (a) (minio `(hello ,a) q))))
(generated-code)
