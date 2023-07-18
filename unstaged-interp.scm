
(defrel (absent-tago v)
  (absento 'struct v))

(defrel (not-tago v)
  (fresh () ;; the fresh is for tests-peano-fib to mysteriously not overfit
    (=/= 'struct v)))

(defrel (booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

(defrel (pos-tago v)
  (== v 'struct))

;; The definition of 'letrec' is based based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.

(defrel (u-evalo expr val)
  (u-eval-expo expr u-initial-env val))

(defrel (u-handle-appo rator rands env val)
  (fresh (a* cfun rep proc prim-id)
    (u-eval-expo rator env cfun)
    (conde
      ((== `(struct prim . ,prim-id) cfun)
       (u-eval-primo prim-id a* val)
       (u-eval-listo rands env a*))
      ((== `(struct closure ,rep) cfun)
       (u-eval-listo rands env a*)
       (callo cfun val a*))
      ((== `(struct rec-closure ,rep) cfun)
       (u-eval-listo rands env a*)
       (callo cfun val a*)))))

(defrel (u-eval-expo expr env val)
  (conde
    ((== `(quote ,val) expr)
     (absent-tago val)
     (u-not-in-envo 'quote env))

    ((numbero expr) (== expr val))

    ((symbolo expr) (u-lookupo expr env val))

    ((fresh (rator rands)
       (== `(,rator . ,rands) expr)
       (u-handle-appo rator rands env val)))
    
    ((fresh (rep x body)
       (== `(lambda ,x ,body) expr)
       (== `(struct closure ,rep) val)
       (conde
         ;; Variadic
         ((symbolo x))
         ;; Multi-argument
         ((u-list-of-symbolso x)))
       (u-not-in-envo 'lambda env)
       (== rep (partial-apply eval-apply x body env))
       ))
    
    ((u-handle-matcho expr env val))

    ((fresh (letrec-body f x e rep)
       (== `(letrec ((,f (lambda ,x ,e)))
              ,letrec-body)
           expr)
       (u-not-in-envo 'letrec env)
       (== rep (partial-apply eval-apply-rec f x e env))
       (u-eval-expo letrec-body
                    `((,f . (val . (struct rec-closure ,rep))) . ,env)
                    val)))
    
    ((u-prim-expo expr env val))
    
    ))

(define u-empty-env '())

(defrel (u-lookupo x env t)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (== `(val . ,t) b))
      ((=/= x y)
       (u-lookupo x rest t)))))

(defrel (u-not-in-envo x env)
  (conde
    ((== u-empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (u-not-in-envo x rest)))))

(defrel (u-eval-listo expr env val)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (u-eval-expo a env v-a)
       (u-eval-listo d env v-d)))))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(defrel (u-list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (u-list-of-symbolso d)))))

(defrel (u-ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((,x . (val . ,a)) . ,env) env2)
       (symbolo x)
       (u-ext-env*o dx* da* env2 out)))))

(defrel (u-eval-primo prim-id a* val)
  (conde
    [(== prim-id 'list)
     (== a* val)]
    [(== prim-id 'cons)
     (fresh (a d)
       (== `(,a ,d) a*)
       (== `(,a . ,d) val))]
    [(== prim-id 'car)
     (fresh (d)
       (== `((,val . ,d)) a*)
       (not-tago val))]
    [(== prim-id 'cdr)
     (fresh (a)
       (== `((,a . ,val)) a*)
       (not-tago a))]
    [(== prim-id 'not)
     (fresh (b)
       (== `(,b) a*)
       (conde
         ((=/= #f b) (== #f val))
         ((== #f b) (== #t val))))]
    [(== prim-id 'equal?)
     (fresh (v1 v2)
       (== `(,v1 ,v2) a*)
       (conde
         ((== v1 v2) (== #t val))
         ((=/= v1 v2) (== #f val))))]
    [(== prim-id 'symbol?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((symbolo v) (== #t val))
         ((numbero v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)))
         ((booleano v) (== #f val))))]
    [(== prim-id 'number?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((numbero v) (== #t val))
         ((symbolo v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)))
         ((booleano v) (== #f val))))]
    [(== prim-id 'pair?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((symbolo v) (== #f val))
         ((numbero v) (== #f val))
         ((booleano v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #t val)
            (not-tago a)))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)
            (pos-tago a)))))]
    [(== prim-id 'null?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((== '() v) (== #t val))
         ((=/= '() v) (== #f val))))]))

(defrel (u-prim-expo expr env val)
  (conde
    ((u-boolean-primo expr env val))
    ((u-and-primo expr env val))
    ((u-or-primo expr env val))
    ((u-if-primo expr env val))))

(defrel (u-boolean-primo expr env val)
  (conde
    ((== #t expr) (== #t val))
    ((== #f expr) (== #f val))))

(defrel (u-and-primo expr env val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (u-not-in-envo 'and env)
    (u-ando e* env val)))

(defrel (u-ando e* env val)
  (conde
    ((== '() e*) (== #t val))
    ((fresh (e)
       (== `(,e) e*)
       (u-eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((== #f v)
          (== #f val)
          (u-eval-expo e1 env v))
         ((=/= #f v)
          (u-eval-expo e1 env v)
          (u-ando `(,e2 . ,e-rest) env val)))))))

(defrel (u-or-primo expr env val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (u-not-in-envo 'or env)
    (u-oro e* env val)))

(defrel (u-oro e* env val)
  (conde
    ((== '() e*) (== #f val))
    ((fresh (e)
       (== `(,e) e*)
       (u-eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((=/= #f v)
          (== v val)
          (u-eval-expo e1 env v))
         ((== #f v)
          (u-eval-expo e1 env v)
          (u-oro `(,e2 . ,e-rest) env val)))))))

(defrel (u-if-primo expr env val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (u-not-in-envo 'if env)
    (u-eval-expo e1 env t)
    (conde
      ((=/= #f t) (u-eval-expo e2 env val))
      ((== #f t) (u-eval-expo e3 env val)))))

(define u-initial-env `((list . (val . (struct prim . list)))
                      (not . (val . (struct prim . not)))
                      (equal? . (val . (struct prim . equal?)))
                      (symbol? . (val . (struct prim . symbol?)))
                      (number? . (val . (struct prim . number?)))
                      (pair? . (val . (struct prim . pair?)))
                      (cons . (val . (struct prim . cons)))
                      (null? . (val . (struct prim . null?)))
                      (car . (val . (struct prim . car)))
                      (cdr . (val . (struct prim . cdr)))
                      . ,u-empty-env))

(defrel (u-handle-matcho expr env val)
  (fresh (against-expr mval clause clauses)
    (== `(match ,against-expr ,clause . ,clauses) expr)
    (not-in-envo 'match env)
    (u-eval-expo against-expr env mval)
    (u-match-clauses mval `(,clause . ,clauses) env val)))

(defrel (u-not-symbolo t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(defrel (u-not-numbero t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((symbolo t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(defrel (u-self-eval-literalo t)
  (conde
    ((numbero t))
    ((u-booleano t))))

(defrel (u-literalo t)
  (conde
    ((numbero t))
    ((symbolo t) (fresh () (not-tago/gen t)))
    ((u-booleano t))
    ((== '() t))))

(defrel (u-booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

(defrel (u-regular-env-appendo env1 env2 env-out)
  (conde
    ((== u-empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (u-regular-env-appendo rest env2 res)))))

(defrel (u-match-clauses mval clauses env val)
  (fresh (p result-expr d penv)
    (== `((,p ,result-expr) . ,d) clauses)
    (conde
      ((fresh (env^)
         (p-match p mval '() penv)
         (regular-env-appendo penv env env^)
         (u-eval-expo result-expr env^ val)))
      ((p-no-match p mval '() penv)
       (u-match-clauses mval d env val)))))

(defrel (u-var-p-match var mval penv penv-out)
  (fresh ()
    (symbolo var)
    (not-tago/gen mval)
    (var-p-match-extend var mval penv penv-out)))

(defrel (u-var-p-match-extend var val penv penv-out)
  (conde
    ((== penv penv-out)
     (u-lookupo var penv val))
    ((== `((,var . (val . ,val)) . ,penv) penv-out)
     (u-not-in-envo var penv))))

(defrel (u-var-p-no-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= mval val)
    (== penv penv-out)
    (u-lookupo var penv val)))

(defrel (u-p-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (== p mval)
     (== penv penv-out))
    ((var-p-match p mval penv penv-out))
    ((fresh (var pred)
      (== `(? ,pred ,var) p)
      (pred-match pred mval)
      (var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (u-quasi-p-match quasi-p mval penv penv-out)))))

(defrel (u-pred-match pred mval)
  (conde
    ((== 'symbol? pred)
     (symbolo mval))
    ((== 'number? pred)
     (numbero mval))))

(defrel (u-p-no-match p mval penv penv-out)
  (conde
    ((u-self-eval-literalo p)
     (=/= p mval)
     (== penv penv-out))
    ((u-var-p-no-match p mval penv penv-out))
    ((fresh (var pred)
       (== `(? ,pred ,var) p)
       (== penv penv-out)
       (symbolo var)
       (u-pred-no-match pred var mval penv penv-out)))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (u-quasi-p-no-match quasi-p mval penv penv-out)))))

(defrel (u-pred-no-match pred var mval penv penv-out)
  (conde
    ((== 'symbol? pred)
     (conde
       ((u-not-symbolo mval))
       ((symbolo mval)
        (u-var-p-no-match var mval penv penv-out))))
    ((== 'number? pred)
     (conde
       ((u-not-numbero mval))
       ((numbero mval)
        (u-var-p-no-match var mval penv penv-out))))))

(defrel (u-quasi-p-match quasi-p mval penv penv-out)
  (conde
    ((== quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
      (== (list 'unquote p) quasi-p)
      (u-p-match p mval penv penv-out)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (== `(,v1 . ,v2) mval)
       (=/= 'unquote a)
       (u-quasi-p-match a v1 penv penv^)
       (u-quasi-p-match d v2 penv^ penv-out)))))

(defrel (u-quasi-p-no-match quasi-p mval penv penv-out)
  (conde
    ((=/= quasi-p mval)
     (== penv penv-out)
     (u-literalo quasi-p))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (not-tago mval)
       (u-p-no-match p mval penv penv-out)))
    ((fresh (a d)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== penv penv-out)
       (u-literalo mval)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== `(,v1 . ,v2) mval)
       (conde
         ((u-quasi-p-no-match a v1 penv penv-out))
         ((u-quasi-p-match a v1 penv penv^)
          (u-quasi-p-no-match d v2 penv^ penv-out)))))))

(defrel (evalo-unstaged expr val)
  (u-eval-expo expr u-initial-env val))
