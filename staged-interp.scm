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

(define (logo f . args)
  (lambda (c)
    (apply printf f (walk* args (c->S c)))
    (newline)
    c))

(define (param-vars args)
  (cond
    ((null? args) '())
    ((symbol? args) (list args))
    (else (cons (car args) (param-vars (cdr args))))))

(define free-vars
  (lambda (bs t . in-cdr)
    (cond ((var? t)
           (if (member t bs)
               '()
               (list t)))
          ((and (null? in-cdr) (pair? t) (eq? (car t) 'lambda))
           (free-vars (append bs (free-vars '()  (cadr t))) (cddr t)))
          ((pair? t) (append (free-vars bs (car t))
                             (free-vars bs (cdr t) #t)))
          (else '()))))

(define (replace-vars t)
  (cond ((var? t)
         (string->symbol (format "x_~a" (var-idx t))))
        ((pair? t)
         (cons (replace-vars (car t))
               (replace-vars (cdr t))))
        (else t)))

(define (remove-list xs lst)
  (if (null? xs)
      lst
      (remove-list (cdr xs) (remove (car xs) lst))))

(define sym-vars
  (lambda (t . in-cdr)
    (cond ((symbol? t)
           (cond
             ((member t '(if fresh conde =/= == quote callo lambda letrec closure quasiquote unquote))
              '())
             ((assoc t initial-env)
              '())
             (else (list t))))
          ((and (null? in-cdr) (pair? t) (eq? (car t) 'lambda))
           (remove-list (sym-vars (cadr t)) (sym-vars (cddr t))))
          ((and (null? in-cdr) (pair? t) (eq? (car t) 'quote))
           '())
          ((pair? t)
           (append (sym-vars (car t))
                   (sym-vars (cdr t) in-cdr)))
          (else '()))))

(define (closure-conversion-eval lam-expr)
  (printf "lam-expr is ~a\n" lam-expr)
  (let* ((bound-vars (param-vars (cadr lam-expr)))
         (free-vars (remove-duplicates (free-vars bound-vars lam-expr)))
         (sym-vars (remove-duplicates (sym-vars lam-expr)))
         (f `(lambda ,sym-vars (lambda ,(replace-vars free-vars) ,(replace-vars lam-expr)))))
    (printf "~a\n" f)
    `(apply (apply (eval ,(list 'quote f))
                   ,(cons 'list sym-vars))
            ,(cons 'list free-vars))))

(define quasi
  (lambda (t)
    (cond
      ((var? t) t)
      ((and (pair? t) (eq? (car t) 'sym)) (cdr t))
      ((and (pair? t) (eq? (car t) 'closure-conversion-eval))
       (closure-conversion-eval (cadr t)))
      ((pair? t) (list 'cons (quasi (car t)) (quasi (cdr t))))
      ((null? t) ''())
      (else (list 'quote t)))))

(define (callo cfun val . a*)
  (fresh ()
    ;;(logo "callo")
    (conde
      ((varo cfun)
       (logo "callo: still var... failing")
       fail
       )
      ((non-varo cfun)
       (conde
         ((fresh (clam cenv ccode)
            (== cfun `(closure ,clam ,cenv ,ccode))
            ;;(logo "callo lambda")
            (lambda (c)
              (((apply (walk* ccode (c->S c)) a*) val)
               c))))
         ((absento 'closure cfun)
          ;;(logo "callo f")
          ;;(logo "callo: ~a ~a ~a" cfun a* val)
          (lambda (c)
            (((apply (walk* cfun (c->S c)) a*) val)
             c))))))))

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

       ((fresh (x body clo-code envt out c-body x^)
          (== `(lambda ,x ,body) expr)
          ;;(logo "closure case")
          ((if stage? l== ==) `(closure (lambda ,x ,body) ,env ,clo-code) val)
          (conde
            ;; Variadic
            ((symbolo x))
            ;; Multi-argument
            ((list-of-symbolso x)))
          (not-in-envo 'lambda env)
          (conde
            ((symbolo x)
             (== x^ (cons 'sym x))
             (== `((,x . (val . ,x^)) . ,env) envt))
            ((list-of-symbolso x)
             (make-list-of-symso x x^)
             (ext-env*o x x^ env envt)))
          (lift-scope
           (eval-expo #t body envt out)
           c-body)
          ;;(== clo-code `(lambda ,x (lambda (,out) (fresh () . ,c-body))))
          (== clo-code `(closure-conversion-eval
                         (lambda ,x (lambda (,out) (fresh () . ,c-body)))))
          ))

       ((fresh (rator x rands body env^ a* res clo-code)
          (== `(,rator . ,rands) expr)
          ;; variadic
          (symbolo x)
          (== `((,x . (val . ,a*)) . ,env^) res)
          (eval-expo #f rator env `(closure (lambda ,x ,body) ,env^ ,clo-code))
          ;;(logo "app closure case ~a" rator)
          (eval-expo stage? body res val)
          (eval-listo rands env a*)))

       ((fresh (rator x* rands body env^ a* res clo-code)
          (== `(,rator . ,rands) expr)
          ;; Multi-argument
          (eval-expo #f rator env `(closure (lambda ,x* ,body) ,env^ ,clo-code))
          ;;(logo "app closure case (multi-arg) ~a" rator)
          (eval-listo rands env a*)
          (ext-env*o x* a* env^ res)
          (eval-expo stage? body res val)))

       ((fresh (rator rands a* p-name)
          (== stage? #t)
          (== `(,rator . ,rands) expr)
          (eval-expo #f rator env `(call ,p-name))
          ;;(logo "app call case ~a" rator)
          (eval-listo rands env a*)
          (lift `(callo ,p-name ,val . ,a*))
          ;;(lift `((,p-name . ,a*) ,val))
          ))

       ((fresh (rator rands a* p-name)
          (== stage? #f)
          (== `(,rator . ,rands) expr)
          (eval-expo #f rator env `(call ,p-name))
          ;;(logo "app call case (unstaged) ~a ~a" rator rands)
          (eval-listo rands env a*)
          (fresh (out)
            (lift `(callo ,p-name ,out . ,a*))
            (== val `(call ,out)))))

       ((fresh (rator rands a* p-name)
            (== stage? #t)
            (== `(,rator . ,rands) expr)
            (symbolo rator)
            (eval-expo #f rator env `(sym . ,p-name))
            ;;(logo "app sym case ~a" rator)
            (eval-listo rands env a*)
            (lift `(callo ,p-name ,val . ,a*))))

       ((fresh (rator rands a* p-name)
            (== stage? #f)
            (== `(,rator . ,rands) expr)
            (symbolo rator)
            (eval-expo #f rator env `(sym . ,p-name))
            ;;(logo "app sym case (unstaged) ~a ~a" rator rands)
            (eval-listo rands env a*)
            (fresh (out)
              (lift `(callo ,p-name ,out . ,a*))
              (== val `(call out)))))

       ((fresh (rator x* rands a* prim-id)
          (== `(,rator . ,rands) expr)
          (eval-expo #f rator env `(prim . ,prim-id))
          (eval-primo prim-id a* val)
          (eval-listo rands env a*)))

       ((handle-matcho expr env val))

       ((fresh (bindings* letrec-body out-bindings* env^)
          ;; single-function variadic letrec version
          (== `(letrec ,bindings*
                 ,letrec-body)
              expr)
          (letrec-bindings-evalo bindings* out-bindings* env env^ env^)
          (not-in-envo 'letrec env)
          (== stage? #t)
          (fresh (c-letrec-body)
            (lift-scope
             (eval-expo #t letrec-body env^ val)
             c-letrec-body)
            (lift `(letrec ,out-bindings*
                     (fresh () . ,c-letrec-body))))))

       ((prim-expo expr env val))))))

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
             (== x^ (cons 'sym x))
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
       (=/= 'closure val))]
    [(== prim-id 'cdr)
     (fresh (a)
       (l== `((,a . ,val)) a*)
       (=/= 'closure a))]
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
                  (=/= a 'closure)))
               ((fresh (a d)
                  (== `(,a . ,d) ,v)
                  (== #f ,val)
                  (== a 'closure))))))]
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
         (=/= 'closure mval)
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

(define (l=/= a b)
  (lift `(=/= ,a ,b)))
