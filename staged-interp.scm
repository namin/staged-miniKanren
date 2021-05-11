(define (groundo x)
  (fresh ()
    (non-varo x)
    (conde
      ((== #t x))
      ((== #f x))
      ((symbolo x))
      ((numbero x))
      ((== x '()))
      ((fresh (a d)
         (== (cons a d) x)
         (groundo a)
         (groundo d))))))

(define (not-groundo x)
  (conde
    ((varo x))
    ((non-varo x)
     (fresh (a d)
       (== (cons a d) x)
       (conde
         ((not-groundo a))
         ((groundo a)
          (not-groundo d)))))))

(define (not-ground-spineo xs)
  (conde
    ((varo xs))
    ((non-varo xs)
     (fresh (xa xd)
       (== (cons xa xd) xs)
       (not-ground-spineo xd)))))

(define (ground-spineo xs)
  (conde
    ((non-varo xs)
     (conde
       ((== xs '()))
       ((fresh (xa xd)
           (== (cons xa xd) xs)
           (ground-spineo xd)))))))

(define (not-ground-paramso xs)
  (conde
    ((varo xs))
    ((non-varo xs)
     (fresh (xa xd)
       (== (cons xa xd) xs)
       (conde
         ((varo xa))
         ((not-ground-paramso xd)))))))

(define (ground-paramso xs)
  (conde
    ((non-varo xs)
     (conde
       ((symbolo xs))
       ((== xs '()))
       ((fresh (xa xd)
          (== (cons xa xd) xs)
          (non-varo xa)
          (ground-paramso xd)))))))

(define (not-letrec-bindings-checko bindings)
  (conde
    ((varo bindings))
    ((non-varo bindings)
     (fresh (b bs p-name x body)
       (== (cons b bs) bindings)
       (conde
         ((varo b))
         ((non-varo b)
          (== b `(,p-name (lambda ,x ,body)))
          (conde
            ((not-ground-paramso (cons p-name x)))
            ((ground-paramso (cons p-name x))
             (not-letrec-bindings-checko bs)))))))))

(define (letrec-bindings-checko bindings)
  (fresh ()
    (non-varo bindings)
    (conde
      ((== '()  bindings))
      ((fresh (b bs p-name x body)
         (== (cons b bs) bindings)
         (== b `(,p-name (lambda ,x ,body)))
         (ground-paramso (cons p-name x))
         (letrec-bindings-checko bs))))))

(define (match-checko clauses)
  (fresh ()
    (non-varo clauses)
    (conde
      ((== '() clauses))
      ((fresh (c cs)
         (== (cons c cs) clauses)
         (match-clause-checko c)
         (match-checko cs))))))

(define (match-clause-checko clause)
  (fresh (scrutiny body)
    (non-varo clause)
    (== (list scrutiny body) clause)
    (groundo scrutiny)))

(define (not-match-checko clauses)
  (conde
    ((varo clauses))
    ((non-varo clauses)
     (fresh (c cs)
       (== (cons c cs) clauses)
       (conde
         ((not-match-clause-checko c))
         ((match-clause-checko c)
          (not-match-checko cs)))))))

(define (not-match-clause-checko clause)
  (conde
    ((varo clause))
    ((non-varo clause)
     (fresh (scrutiny body)
       (== (list scrutiny body) clause)
       (not-groundo scrutiny)))))

(define (not-tago v)
  (fresh ()
    (=/= 'closure v)
    (=/= 'prim v)
    (=/= 'call v)
    (=/= 'dynamic v)))

(define (absent-staged-tago v)
  (fresh ()
    (absento 'call v)
    (absento 'dynamic v)))

(define (mapo fo xs ys)
  (conde
    ((== xs '()) (== ys '()))
    ((fresh (xa xd ya yd)
       (== xs (cons xa xd))
       (== ys (cons ya yd))
       (fo xa ya)
       (mapo fo xd yd)))))

(define (make-list-of-symso xs ys)
  (mapo (lambda (x y) (== y (unexpand x))) xs ys))

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

(define (logo f . args)
  (lambda (c)
    (apply printf f (walk* args (c->S c)))
    (newline)
    c))

(define (callo cfun val a*)
  (conde
    ((fresh (clam cenv ccode)
       (== cfun `(closure ,clam ,cenv ,ccode))
       (lambda (c)
         (((maybe-apply (walk* ccode (c->S c)) (walk* cfun (c->S c)) (walk* a* (c->S c))) val)
          c))))
    ((absento 'closure cfun)
     (lambda (c)
       (((maybe-apply (walk* cfun (c->S c)) (walk* cfun (c->S c)) (walk* a* (c->S c))) val)
        c)))))

(define maybe-apply
  (lambda (cfun whole a*)
    (lambda (val)
      (if (and (procedure? cfun) (list? a*))
          (call/cc
           (lambda (k)
             (with-exception-handler
              (lambda (_) (k fail))
              (lambda () ((apply cfun a*) val)))))
          (conde
            ((fresh (clam cenv ccode x* body env^)
               (== whole `(closure ,clam ,cenv ,ccode))
               (== clam `(lambda ,x* ,body))
               (conde
                 ((symbolo x*)
                  (== `((,x* . (val . ,a*)) . ,cenv) env^))
                 ((u-ext-env*o x* a* cenv env^)))
               (u-eval-expo body env^ val)))
            ((absento 'closure whole)
             fail))))))

(define (eval-expo stage? expr env val)
  (conde
    ((varo expr)
     (absent-staged-tago val)
     (lift `(u-eval-expo ,(expand expr) ,(expand env) ,(expand val))))
    ((non-varo expr)
     (conde
       ((numbero expr) ((if stage? l== ==) expr val))

       ((symbolo expr) (lookupo stage? expr env val))

       ((fresh (rator rands)
          (== `(,rator . ,rands) expr)
          (conde
            ((conde
               ((varo rator))
               ((non-varo rator) (not-ground-spineo rands)))
             (absent-staged-tago val)
             (lift `(u-eval-expo ,(expand expr) ,(expand env) ,(expand val))))
            ((ground-spineo rands)
             (non-varo rator)
             (conde
               ((fresh (v)
                  (== `(quote ,v) expr)
                  (absento 'closure v)
                  (absento 'prim v)
                  (absento 'call v)
                  (absento 'dynamic v)
                  (not-in-envo 'quote env)
                  ((if stage? l== ==) val v)))
               ((fresh (x body clo-code envt out c-body x^)
                  (== `(lambda ,x ,body) expr)
                  ((if stage? l== ==) `(closure (lambda ,x ,body) ,env ,clo-code) val)
                  (conde
                    ((not-ground-paramso x)
                     (absent-staged-tago val)
                     (lift `(u-eval-expo ,(expand expr) ,(expand env) ,(expand val))))
                    ((ground-paramso x)
                     (conde
                       ;; Variadic
                       ((symbolo x))
                       ;; Multi-argument
                       ((list-of-symbolso x)))
                     (not-in-envo 'lambda env)
                     (conde
                       ((symbolo x)
                        (== x^ (unexpand x))
                        (== `((,x . (val . ,x^)) . ,env) envt))
                       ((list-of-symbolso x)
                        (make-list-of-symso x x^)
                        (ext-env*o x x^ env envt)))
                     (lift-scope
                      (eval-expo #t body envt out)
                      c-body)
                     (== clo-code (unexpand `(lambda ,x (lambda (,out) (fresh () . ,c-body)))))))))
               ((fresh (proc)
                  (eval-expo #f rator env proc)
                  (conde
                    ((varo proc)
                     (lift `(u-eval-expo ,(expand expr) ,(expand env) ,(expand val))))
                    ((non-varo proc)
                     (conde
                       ((fresh (x* body env^ a* res clo-code)
                          (== proc `(closure (lambda ,x* ,body) ,env^ ,clo-code))
                          (conde
                            ((not-ground-paramso x*)
                             (lift `(u-eval-expo ,(expand expr) ,(expand env) ,(expand val))))
                            ((ground-paramso x*)
                             (conde
                               ;; Variadic
                               ((symbolo x*)
                                (== `((,x* . (val . ,a*)) . ,env^) res)
                                (eval-expo stage? body res val)
                                (eval-listo rands env a*))
                               ;; Multi-argument
                               ((eval-listo rands env a*)
                                (ext-env*o x* a* env^ res)
                                (eval-expo stage? body res val)))))))
                       ((fresh (a* p-name)
                          (== stage? #t)
                          (== proc `(call ,p-name))
                          (eval-listo rands env a*)
                          (lift `(callo ,p-name ,(expand val) ,(expand a*)))))
                       ((fresh (a* p-name)
                          (== stage? #t)
                          (== proc `(dynamic ,p-name))
                          (eval-listo rands env a*)
                          (lift `(callo ,p-name ,(expand val) ,(expand a*)))))
                       ((fresh (a* p-name)
                          (== stage? #t)
                          (symbolo rator)
                          (== proc (unexpand p-name))
                          (eval-listo rands env a*)
                          (lift `(callo ,p-name ,(expand val) ,(expand a*)))))
                       ((fresh (prim-id a*)
                          (== proc `(prim . ,prim-id))
                          (non-varo prim-id)
                          (eval-primo prim-id a* val)
                          (eval-listo rands env a*))))))))
               ((handle-matcho expr env val))
               ((fresh (bindings* letrec-body out-bindings* env^)
                  ;; single-function variadic letrec version
                  (== `(letrec ,bindings*
                         ,letrec-body)
                      expr)
                  (conde
                    ((not-letrec-bindings-checko bindings*)
                     (lift `(u-eval-expo ,(expand expr) ,(expand env) ,(expand val))))
                    ((letrec-bindings-checko bindings*)
                     (letrec-bindings-evalo bindings* out-bindings* env env^ env^)
                     (not-in-envo 'letrec env)
                     (== stage? #t)
                     (fresh (c-letrec-body)
                       (lift-scope
                        (eval-expo #t letrec-body env^ val)
                        c-letrec-body)
                       (lift `(letrec ,out-bindings*
                                (fresh () . ,c-letrec-body))))))))
               ((prim-expo expr env val))
               )))))

       ((boolean-primo expr env val))

       ((fresh (rator rands a* p-name)
          (== stage? #f)
          (== `(,rator . ,rands) expr)
          (eval-expo #f rator env `(call ,p-name))
          (eval-listo rands env a*)
          (fresh (out)
            (lift `(callo ,p-name ,(expand out) ,(expand a*)))
            (== val `(dynamic ,out)))))

       ((fresh (rator rands a* p-name)
          (== stage? #f)
          (== `(,rator . ,rands) expr)
          (eval-expo #f rator env `(dynamic ,p-name))
          (eval-listo rands env a*)
          (fresh (out)
            (lift `(callo ,p-name ,(expand out) ,(expand a*)))
            (== val `(dynamic ,out)))))

       ))))

(define (letrec-bindings-evalo bindings* out-bindings* env envt env^)
  (conde
    ((== '() bindings*)
     (== '() out-bindings*)
     (== env env^))
    ((fresh (b bs e es res out c-body o os p-name x body x^)
       (== (cons b bs) bindings*)
       (== b `(,p-name (lambda ,x ,body)))
       (== (cons e es) env^)
       (== e `(,p-name . (rec . (lambda ,x ,body))))
       (== (cons o os) out-bindings*)
       (letrec-bindings-evalo bs os env envt es)
       (conde
            ((symbolo x)
             (== x^ (unexpand x))
             (== `((,x . (val . ,x^)) . ,envt) res))
            ((list-of-symbolso x)
             (make-list-of-symso x x^)
             (ext-env*o x x^ envt res)))
       (lift-scope
        (eval-expo #t body res out)
        c-body)
       (== o `(,p-name (lambda ,x (lambda (,out) (fresh () . ,c-body)))))))))

(define empty-env '())

(define (lookupo stage? x env t)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (conde
         ((fresh (v) (== `(val . ,v) b) ((if stage? l== ==) v t)))
         ((fresh (lam-expr)
            (== `(rec . ,lam-expr) b)
            ((if stage? l== ==) `(call ,x) t)))))
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
       (not-tago val))]
    [(== prim-id 'cdr)
     (fresh (a)
       (l== `((,a . ,val)) a*)
       (not-tago a))]
    [(== prim-id 'not)
     (fresh (b)
       (l== `(,b) a*)
       (lift `(conde
               ((=/= #f ,b) (== #f ,val))
               ((== #f ,b) (== #t ,val)))))]
    [(== prim-id 'equal?)
     (fresh (v1 v2)
       (l== `(,v1 ,v2) a*)
       (lift `(conde
               ((== ,v1 ,v2) (== #t ,val))
               ((=/= ,v1 ,v2) (== #f ,val)))))]
    [(== prim-id 'symbol?)
     (fresh (v)
       (l== `(,v) a*)
       (lift `(conde
               ((symbolo ,v) (== #t ,val))
               ((numbero ,v) (== #f ,val))
               ((fresh (a d)
                  (== `(,a . ,d) ,v)
                  (== #f ,val))))))]
    [(== prim-id 'number?)
     (fresh (v)
       (l== `(,v) a*)
       (lift `(conde
         ((numbero ,v) (== #t ,val))
         ((symbolo ,v) (== #f ,val))
         ((fresh (a d)
            (== `(,a . ,d) ,v)
            (== #f ,val))))))]
    [(== prim-id 'pair?)
     (fresh (v)
       (l== `(,v) a*)
       (lift `(conde
               ((symbolo ,v) (== #f ,val))
               ((numbero ,v) (== #f ,val))
               ((fresh (a d)
                  (== `(,a . ,d) ,v)
                  (== #t ,val)
                  (not-tago a)))
               ((fresh (a d)
                  (== `(,a . ,d) ,v)
                  (== #f ,val)
                  (conde
                    ((== a 'closure))
                    ((== a 'prim))
                    ((== a 'call))
                    ((== a 'dynamic))))))))]
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
    ((if-primo expr env val))
    ((choice-primo expr env val))
    ))

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

(define (choice-primo expr env val)
  (fresh (e2 e3 c2 c3)
    (== `(choice ,e2 ,e3) expr)
    (not-in-envo 'choice env)
    (lift-scope (eval-expo #t e2 env val) c2)
    (lift-scope (eval-expo #t e3 env val) c3)
    (lift `(conde
            ,c2
            ,c3))))

(define initial-env `((list . (val . (closure (lambda x x) ,empty-env (lambda x (lambda (out) (== x out))))))
                      (not . (val . (prim . not)))
                      (equal? . (val . (prim . equal?)))
                      (symbol? . (val . (prim . symbol?)))
		      (number? . (val . (prim . number?)))
                      (pair? . (val . (prim . pair?)))
                      (cons . (val . (prim . cons)))
                      (null? . (val . (prim . null?)))
                      (car . (val . (prim . car)))
                      (cdr . (val . (prim . cdr)))
                      . ,empty-env))

(define handle-matcho
  (lambda  (expr env val)
    (fresh (against-expr clause clauses)
      (== `(match ,against-expr ,clause . ,clauses) expr)
      (conde
        ((not-match-checko (cons clause clauses))
         (lift `(u-eval-expo ,(expand expr) ,(expand env) ,(expand val))))
        ((match-checko (cons clause clauses))
         (fresh (mval)
           (not-in-envo 'match env)
           (eval-expo #t against-expr env mval)
           (match-clauses mval `(,clause . ,clauses) env val)))))))

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
    ((symbolo t) (not-tago t))
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
    (not-tago mval)
    (l== mval val)
    (conde
      ((== penv penv-out)
       (lookupo #t var penv val))
      ((== `((,var . (val . ,val)) . ,penv) penv-out)
       (not-in-envo var penv)))))

(define (var-p-no-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (l=/= mval val)
    (== penv penv-out)
    ;; This lookup is for linear pattern matching
    ;; which is not supported by staging,
    ;; since penv is not reified.
    ;;(lookupo #t var penv val)
    ))

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
         (lift `(symbolo ,mval)))
        ((== 'number? pred)
         (lift `(numbero ,mval))))
      (var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-match quasi-p mval penv penv-out)))))

(define (p-no-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (l=/= p mval)
     (== penv penv-out))
    ((var-p-no-match p mval penv penv-out))
    ((fresh (var pred val)
       (== `(? ,pred ,var) p)
       (== penv penv-out)
       (symbolo var)
       (conde
         ((== 'symbol? pred)
          (fresh (z1 z2)
            (lift-scope
             (fresh ()
               (lift `(not-symbolo ,mval)))
             z1)
            (lift-scope
             (fresh ()
               (lift `(symbolo ,mval))
               (var-p-no-match var mval penv penv-out))
             z2)
            (lift `(conde ,z1 ,z2))))
         ((== 'number? pred) ;; TODO: same pattern as symbol? above
          (conde
            ((lift `(not-numbero ,mval)))
            ((lift `(numbero ,mval))
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
  (fresh ()
    (conde
      ((l=/= quasi-p mval)
       (== penv penv-out)
       (literalo quasi-p))
      ((fresh (p)
         (== (list 'unquote p) quasi-p)
         (not-tago mval)
         (p-no-match p mval penv penv-out)))
      ((fresh (a d v1 v2 penv^)
         (== `(,a . ,d) quasi-p)
         (=/= 'unquote a)
         (fresh (z1 z2)
           (lift-scope
            (fresh ()
              (== penv penv-out)
              (lift `(literalo ,mval)))
            z1)
           (lift-scope
            (fresh ()
              (l== `(,v1 . ,v2) mval)
              (conde
                ((quasi-p-no-match a v1 penv penv^))
                ((quasi-p-match a v1 penv penv^)
                 (quasi-p-no-match d v2 penv^ penv-out))))
            z2)
           (lift `(conde ,z1 ,z2))))))))
