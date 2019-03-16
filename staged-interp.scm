(define (mapo fo xs ys)
  (conde
    ((== xs '()) (== ys '()))
    ((fresh (xa xd ya yd)
       (== xs (cons xa xd))
       (== ys (cons ya yd))
       (fo xa ya)
       (mapo fo xd yd)))))

(define (make-list-of-symso xs ys)
  (mapo (lambda (x y) (== y (cons 'sym x))) xs ys))

(define (varo x)
  (lambda (c)
    (if (var? (walk* x (c->S c)))
        c
        #f)))

(define (non-varo x)
  (lambda (c)
    (if (var? (walk* x (c->S c)))
        #f
        c)))

(define (eval-expo stage? expr env val)
  (conde
    ((== stage? #t) (varo expr)
     (lambda (c)
       ((lift `(u-eval-expo ,expr ,(quasi (walk* env (c->S c))) ,val))
        c)))
    ((conde
       ((non-varo expr))
       ((== stage? #f)))
     (conde
       ((fresh (v)
          (== `(quote ,v) expr)
          (absento 'closure v)
          (absento 'prim v)
          (not-in-envo 'quote env)
          ((if stage? l== ==) val v)))

       ((numbero expr) ((if stage? l== ==) expr val))

       ((symbolo expr) (lookupo stage? expr env val))

       ((fresh (x body)
          (== `(lambda ,x ,body) expr)
          (== `(closure (lambda ,x ,body) ,env) val)
          (conde
            ;; Variadic
            ((symbolo x))
            ;; Multi-argument
            ((list-of-symbolso x)))
          (not-in-envo 'lambda env)))
       
       ((fresh (rator x rands body env^ a* res)
          (== `(,rator . ,rands) expr)
          ;; variadic
          (symbolo x)
          (== `((,x . (val . ,a*)) . ,env^) res)
          (eval-expo #f rator env `(closure (lambda ,x ,body) ,env^))
          (eval-expo stage? body res val)
          (eval-listo rands env a*)))

       ((fresh (rator x* rands body env^ a* res)
          (== `(,rator . ,rands) expr)
          ;; Multi-argument
          (eval-expo #f rator env `(closure (lambda ,x* ,body) ,env^))
          (eval-listo rands env a*)
          (ext-env*o x* a* env^ res)
          (eval-expo stage? body res val)))

       ((fresh (rator rands a* p-name)
          (== stage? #t)
          (== `(,rator . ,rands) expr)
          (eval-expo #f rator env `(call ,p-name))
          (eval-listo rands env a*)
          (lift `((,p-name . ,a*) ,val))))

       ((fresh (rator x* rands a* prim-id)
          (== `(,rator . ,rands) expr)
          (eval-expo #f rator env `(prim . ,prim-id))
          (eval-primo prim-id a* val)
          (eval-listo rands env a*)))
       
       ((handle-matcho expr env val))

       ((fresh (p-name x body letrec-body x^ res env^)
          ;; single-function variadic letrec version
          (== `(letrec ((,p-name (lambda ,x ,body)))
                 ,letrec-body)
              expr)
          (== env^ `((,p-name . (rec ,stage? . (lambda ,x ,body))) . ,env))
          (conde
                                        ; Variadic
            ((symbolo x)
             (== x^ (cons 'sym x))
             (== `((,x . (val . ,x^)) . ,env^) res))
                                        ; Multiple argument
            ((list-of-symbolso x)
             (make-list-of-symso x x^)
             (ext-env*o x x^ env^ res)))
          (not-in-envo 'letrec env)
          (conde
            ((== stage? #t)
             (fresh (out c-body c-letrec-body)
               (lift-scope
                (eval-expo #t body res out)
                c-body)
               (lift-scope
                (eval-expo #t letrec-body env^ val)
                c-letrec-body)
               (lift `(letrec ((,p-name (lambda ,x (lambda (,out) (fresh () . ,c-body)))))
                        (fresh () . ,c-letrec-body))))
             )
            ((== stage? #f)
             (eval-expo stage? letrec-body env^ val)))))

       ((prim-expo expr env val))
       
       ))))

(define empty-env '())

(define (lookupo stage? x env t)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (conde
         ((fresh (v) (== `(val . ,v) b) ((if stage? l== ==) v t)))
         ((fresh (rec-fold? lam-expr)
            (== `(rec ,rec-fold? . ,lam-expr) b)
            (conde
              ((== rec-fold? #t) ((if stage? l== ==) `(call ,x) t))
              ((== rec-fold? #f) ((if stage? l== ==) `(closure ,lam-expr ,env) t)))))))
      ((=/= x y)
       (lookupo stage? x rest t)))))

(define (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))

(define (eval-listo expr env val)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expo #t a env v-a)
       (eval-listo d env v-d)))))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(define (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(define (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((,x . (val . ,a)) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

(define (eval-primo prim-id a* val)
  (conde
    [(== prim-id 'cons)
     (fresh (a d)
       (l== `(,a ,d) a*)
       (l== `(,a . ,d) val))]
    [(== prim-id 'car)
     (fresh (d)
       (l== `((,val . ,d)) a*)
       (=/= 'closure val))]
    [(== prim-id 'cdr)
     (fresh (a)
       (l== `((,a . ,val)) a*)
       (=/= 'closure a))]
    [(== prim-id 'not) ;; TODO: stage
     (fresh (b)
       (== `(,b) a*)
       (conde
         ((=/= #f b) (== #f val))
         ((== #f b) (== #t val))))]
    [(== prim-id 'equal?)
     (fresh (v1 v2)
       (l== `(,v1 ,v2) a*)
       (lift `(conde
               ((== ,v1 ,v2) (== #t ,val))
               ((=/= ,v1 ,v2) (== #f ,val)))))]
    [(== prim-id 'symbol?) ;; TODO: stage
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((symbolo v) (== #t val))
         ((numbero v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)))))]
    [(== prim-id 'null?)
     (fresh (v)
       (l== `(,v) a*)
       (lift `(conde
               ((== '() ,v) (== #t ,val))
               ((=/= '() ,v) (== #f ,val)))))]))

(define (prim-expo expr env val)
  (conde
    ((boolean-primo expr env val))
    ((and-primo expr env val))
    ((or-primo expr env val))
    ((if-primo expr env val))))

(define (boolean-primo expr env val)
  (conde
    ((== #t expr) (l== #t val))
    ((== #f expr) (l== #f val))))

(define (and-primo expr env val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (not-in-envo 'and env)
    (ando e* env val)))

(define (ando e* env val)
  (conde
    ((== '() e*) (== #t val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo #t e env val)))
    ((fresh (e1 e2 e-rest v c)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (eval-expo #t e1 env v)
       (lift-scope
        (ando `(,e2 . ,e-rest) env val)
        c)
       (lift `(conde
                ((== #f ,v)
                 (== #f ,val))
                ((=/= #f ,v)
                 . ,c)))))))

(define (or-primo expr env val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (not-in-envo 'or env)
    (oro e* env val)))

(define (oro e* env val)
  (conde
    ((== '() e*) (== #f val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo #t e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((=/= #f v)
          (== v val)
          (eval-expo #t e1 env v))
         ((== #f v)
          (eval-expo #t e1 env v)
          (oro `(,e2 . ,e-rest) env val)))))))

(define (if-primo expr env val)
  (fresh (e1 e2 e3 t c2 c3)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    (eval-expo #t e1 env t)
    (lift-scope (eval-expo #t e2 env val) c2)
    (lift-scope (eval-expo #t e3 env val) c3)
    (lift `(conde
            ((=/= #f ,t) . ,c2)
            ((== #f ,t) . ,c3)))))

(define initial-env `((list . (val . (closure (lambda x x) ,empty-env)))
                      (not . (val . (prim . not)))
                      (equal? . (val . (prim . equal?)))
                      (symbol? . (val . (prim . symbol?)))
                      (cons . (val . (prim . cons)))
                      (null? . (val . (prim . null?)))
                      (car . (val . (prim . car)))
                      (cdr . (val . (prim . cdr)))
                      . ,empty-env))

(define handle-matcho
  (lambda  (expr env val)
    (fresh (against-expr mval clause clauses)
      (== `(match ,against-expr ,clause . ,clauses) expr)
      (not-in-envo 'match env)
      (eval-expo #t against-expr env mval)
      (match-clauses mval `(,clause . ,clauses) env val))))

(define (not-symbolo t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (not-numbero t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((symbolo t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (self-eval-literalo t)
  (conde
    ((numbero t))
    ((booleano t))))

(define (literalo t)
  (conde
    ((numbero t))
    ((symbolo t) (=/= 'closure t))
    ((booleano t))
    ((== '() t))))

(define (booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

(define (regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

(define (match-clauses mval clauses env val)
  (conde
    ((== clauses '())
     (lift 'fail))
    ((fresh (p result-expr d penv c-yes c-no)
       (== `((,p ,result-expr) . ,d) clauses)
       (lift-scope
        (fresh (env^)
          (p-match p mval '() penv)
          (regular-env-appendo penv env env^)
          (eval-expo #t result-expr env^ val))
        c-yes)
       (lift-scope
        (fresh ()
          (p-no-match p mval '() penv)
          (match-clauses mval d env val))
        c-no)
       (lift `(conde ,c-yes ,c-no))))))

(define (var-p-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= 'closure mval)
    (l== mval val)
    (conde
      ((== penv penv-out)
       (lookupo #f var penv val))
      ((== `((,var . (val . ,val)) . ,penv) penv-out)
       (not-in-envo var penv)))))

(define (var-p-no-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= mval val)
    (== penv penv-out)
    (lookupo #f var penv val)))

(define (p-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (l== p mval)
     (== penv penv-out))
    ((var-p-match p mval penv penv-out))
    ((fresh (var pred val)
      (== `(? ,pred ,var) p)
      (conde
        ((== 'symbol? pred)
         (symbolo mval))
        ((== 'number? pred)
         (numbero mval)))
      (var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-match quasi-p mval penv penv-out)))))

(define (p-no-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (=/= p mval)
     (== penv penv-out))
    ((var-p-no-match p mval penv penv-out))
    ((fresh (var pred val)
       (== `(? ,pred ,var) p)
       (== penv penv-out)
       (symbolo var)
       (conde
         ((== 'symbol? pred)
          (conde
            ((not-symbolo mval))
            ((symbolo mval)
             (var-p-no-match var mval penv penv-out))))
         ((== 'number? pred)
          (conde
            ((not-numbero mval))
            ((numbero mval)
             (var-p-no-match var mval penv penv-out)))))))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-no-match quasi-p mval penv penv-out)))))

(define (quasi-p-match quasi-p mval penv penv-out)
  (conde
    ((l== quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
      (== (list 'unquote p) quasi-p)
      (p-match p mval penv penv-out)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (l== `(,v1 . ,v2) mval)
       (=/= 'unquote a)
       (quasi-p-match a v1 penv penv^)
       (quasi-p-match d v2 penv^ penv-out)))))

(define (quasi-p-no-match quasi-p mval penv penv-out)
  (conde
    ((=/= quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (=/= 'closure mval)
       (p-no-match p mval penv penv-out)))
    ((fresh (a d)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== penv penv-out)
       (literalo mval)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (l== `(,v1 . ,v2) mval)
       (conde
         ((quasi-p-no-match a v1 penv penv^))
         ((quasi-p-match a v1 penv penv^)
          (quasi-p-no-match d v2 penv^ penv-out)))))))
