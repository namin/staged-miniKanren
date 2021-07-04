;; The definition of 'letrec' is based based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.

(define (u-evalo expr val)
  (u-eval-expo expr u-initial-env val))

(define (u-eval-expo expr env val)
  (conde
    ((== `(quote ,val) expr)
     (absento 'closure val)
     (absento 'prim val)
     (absento 'call val)
     (absento 'dynamic val)

     (u-not-in-envo 'quote env))

    ((numbero expr) (== expr val))

    ((symbolo expr) (u-lookupo expr env val))

    ((fresh (x body extra)
       (== `(lambda ,x ,body) expr)
       (== `(closure (lambda ,x ,body) ,env ,extra) val)
       (conde
         ;; Variadic
         ((symbolo x))
         ;; Multi-argument
         ((u-list-of-symbolso x)))
       (u-not-in-envo 'lambda env)))
    
    ((fresh (rator x* rands body env^ a* cfun extra)
       (== `(,rator . ,rands) expr)
       (u-eval-expo rator env cfun)
       (== `(closure (lambda ,x* ,body) ,env^ ,extra) cfun)
       (u-eval-listo rands env a*)
       (callo cfun val a*)))

    ((fresh (rator x* rands a* prim-id)
       (== `(,rator . ,rands) expr)
       (u-eval-expo rator env `(prim . ,prim-id))
       (u-eval-primo prim-id a* val)
       (u-eval-listo rands env a*)))
    
    ((u-handle-matcho expr env val))

    ((fresh (p-name x body letrec-body)
       ;; single-function variadic letrec version
       (== `(letrec ((,p-name (lambda ,x ,body)))
              ,letrec-body)
           expr)
       (conde
         ; Variadic
         ((symbolo x))
         ; Multiple argument
         ((u-list-of-symbolso x)))
       (u-not-in-envo 'letrec env)
       (u-eval-expo letrec-body
                  `((,p-name . (rec . (lambda ,x ,body))) . ,env)
                  val)))
    
    ((u-prim-expo expr env val))
    
    ))

(define u-empty-env '())

(define (u-lookupo x env t)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (conde
         ((== `(val . ,t) b))
         ((fresh (lam-expr extra)
            (== `(rec . ,lam-expr) b)
            (== `(closure ,lam-expr ,env ,extra) t)))))
      ((=/= x y)
       (u-lookupo x rest t)))))

(define (u-not-in-envo x env)
  (conde
    ((== u-empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (u-not-in-envo x rest)))))

(define (u-eval-listo expr env val)
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
(define (u-list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (u-list-of-symbolso d)))))

(define (u-ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((,x . (val . ,a)) . ,env) env2)
       (symbolo x)
       (u-ext-env*o dx* da* env2 out)))))

(define (u-eval-primo prim-id a* val)
  (conde
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
            (== #f val)))))]
    [(== prim-id 'number?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((numbero v) (== #t val))
         ((symbolo v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)))))]
    [(== prim-id 'pair?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((symbolo v) (== #f val))
         ((numbero v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #t val)
            (not-tago a)))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)
            (conde
              ((== a 'closure))
              ((== a 'prim))
              ((== a 'call))
              ((== a 'dynamic)))))))]
    [(== prim-id 'null?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((== '() v) (== #t val))
         ((=/= '() v) (== #f val))))]))

(define (u-prim-expo expr env val)
  (conde
    ((u-boolean-primo expr env val))
    ((u-and-primo expr env val))
    ((u-or-primo expr env val))
    ((u-if-primo expr env val))))

(define (u-boolean-primo expr env val)
  (conde
    ((== #t expr) (== #t val))
    ((== #f expr) (== #f val))))

(define (u-and-primo expr env val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (u-not-in-envo 'and env)
    (u-ando e* env val)))

(define (u-ando e* env val)
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

(define (u-or-primo expr env val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (u-not-in-envo 'or env)
    (u-oro e* env val)))

(define (u-oro e* env val)
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

(define (u-if-primo expr env val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (u-not-in-envo 'if env)
    (u-eval-expo e1 env t)
    (conde
      ((=/= #f t) (u-eval-expo e2 env val))
      ((== #f t) (u-eval-expo e3 env val)))))

(define u-initial-env `((list . (val . (closure (lambda x x) ,u-empty-env (lambda x (lambda (out) (== x out))))))
                      (not . (val . (prim . not)))
                      (equal? . (val . (prim . equal?)))
                      (symbol? . (val . (prim . symbol?)))
                      (number? . (val . (prim . number?)))
                      (pair? . (val . (prim . pair?)))
                      (cons . (val . (prim . cons)))
                      (null? . (val . (prim . null?)))
                      (car . (val . (prim . car)))
                      (cdr . (val . (prim . cdr)))
                      . ,u-empty-env))

(define u-handle-matcho
  (lambda  (expr env val)
    (fresh (against-expr mval clause clauses)
      (== `(match ,against-expr ,clause . ,clauses) expr)
      (u-not-in-envo 'match env)
      (u-eval-expo against-expr env mval)
      (u-match-clauses mval `(,clause . ,clauses) env val))))

(define (u-not-symbolo t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (u-not-numbero t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((symbolo t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (u-self-eval-literalo t)
  (conde
    ((numbero t))
    ((u-booleano t))))

(define (u-literalo t)
  (conde
    ((numbero t))
    ((symbolo t) (not-tago t))
    ((u-booleano t))
    ((== '() t))))

(define (u-booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

(define (u-regular-env-appendo env1 env2 env-out)
  (conde
    ((== u-empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (u-regular-env-appendo rest env2 res)))))

(define (u-match-clauses mval clauses env val)
  (fresh (p result-expr d penv)
    (== `((,p ,result-expr) . ,d) clauses)
    (conde
      ((fresh (env^)
         (u-p-match p mval '() penv)
         (u-regular-env-appendo penv env env^)
         (u-eval-expo result-expr env^ val)))
      ((u-p-no-match p mval '() penv)
       (u-match-clauses mval d env val)))))

(define (u-var-p-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (not-tago mval)
    (conde
      ((== mval val)
       (== penv penv-out)
       (u-lookupo var penv val))
      ((== `((,var . (val . ,mval)) . ,penv) penv-out)
       (u-not-in-envo var penv)))))

(define (u-var-p-no-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= mval val)
    (== penv penv-out)
    (u-lookupo var penv val)))

(define (u-p-match p mval penv penv-out)
  (conde
    ((u-self-eval-literalo p)
     (== p mval)
     (== penv penv-out))
    ((u-var-p-match p mval penv penv-out))
    ((fresh (var pred val)
      (== `(? ,pred ,var) p)
      (conde
        ((== 'symbol? pred)
         (symbolo mval))
        ((== 'number? pred)
         (numbero mval)))
      (u-var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (u-quasi-p-match quasi-p mval penv penv-out)))))

(define (u-p-no-match p mval penv penv-out)
  (conde
    ((u-self-eval-literalo p)
     (=/= p mval)
     (== penv penv-out))
    ((u-var-p-no-match p mval penv penv-out))
    ((fresh (var pred val)
       (== `(? ,pred ,var) p)
       (== penv penv-out)
       (symbolo var)
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
             (u-var-p-no-match var mval penv penv-out)))))))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (u-quasi-p-no-match quasi-p mval penv penv-out)))))

(define (u-quasi-p-match quasi-p mval penv penv-out)
  (conde
    ((== quasi-p mval)
     (== penv penv-out)
     (u-literalo quasi-p))
    ((fresh (p)
      (== (list 'unquote p) quasi-p)
      (u-p-match p mval penv penv-out)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (== `(,v1 . ,v2) mval)
       (=/= 'unquote a)
       (u-quasi-p-match a v1 penv penv^)
       (u-quasi-p-match d v2 penv^ penv-out)))))

(define (u-quasi-p-no-match quasi-p mval penv penv-out)
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
