(define (eval-apply-rec-staged rep f x e env arg res)
  (fresh (env^)
    (== env^ `((,x . (val . ,arg)) (,f . (val . (rec-closure ,rep))) . ,env))
    (eval-expo e env^ res)))

(define (eval-apply-rec-dyn f x e env arg res)
  (error 'eval-apply-rec-dyn "shouldn't be called"))

(define (eval-apply-staged rep x* body env a* val)
  (fresh (env^)
    (conde
      ((symbolo x*)
       (== `((,x* . (val . ,a*)) . ,env) env^))
      ((list-of-symbolso x*)
       (ext-env*o x* a* env env^)))
    (eval-expo body env^ val)))

(define (eval-apply-dyn x* body env a* val)
  (fresh (env^)
    (conde
      ((symbolo x*)
       (== `((,x* . (val . ,a*)) . ,env) env^))
      ((u-ext-env*o x* a* env env^)))
    (u-eval-expo body env^ val)))

(define (booleano x)
  (conde
    ((== x #t))
    ((== x #f))))

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
         ((non-varo xa)
          (not-ground-paramso xd)))))))

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
    (=/= 'dynamic v)
    (=/= 'call-code v)))

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
    (if (var? (walk* x (state-S c)))
        c
        #f)))

(define (non-varo x)
  (lambda (c)
    (if (var? (walk* x (state-S c)))
        #f
        c)))

(define (logo f . args)
  (lambda (c)
    (apply printf f (walk* args (state-S c)))
    (newline)
    c))

(define (same-lengtho a* b*)
  (conde
    ((== a* '()) (== b* '()))
    ((fresh (a ar b br)
       (== a* (cons a ar))
       (== b* (cons b br))
       (same-lengtho ar br)))))

(define (callo proc val a*)
  (conde
    ((fresh (rep)
       (== proc `(closure ,rep))
       (apply-reified rep ((eval-apply-staged eval-apply-dyn) (_ _ _) (a* val)))))
    ((fresh (rep a)
       (== proc `(rec-closure ,rep))
       (== `(,a) a*)
       (logo "here")
       (apply-reified rep ((eval-apply-rec-staged eval-apply-rec-dyn) (_ _ _ _) (a val)))
       (logo "there")))
    ((fresh (prim-id)
       (== proc `(prim . ,prim-id))
       (u-eval-primo prim-id a* val)))
    ((fresh (code)
       (== proc `(call ,code))
       (project (code a*)
         (if (and (procedure? code) (list? a*)) ;; TODO: what if a* is a var?
             (call/cc
              (lambda (k)
                (with-exception-handler
                 (lambda (_) (k fail))
                 (lambda () ((apply code a*) val)))))
             fail))))))

(define (eval-expo expr env val)
  (conde
    ((varo expr)
     (later `(u-eval-expo ,(expand expr) ,(expand env) ,(expand val))))
    ((non-varo expr)
     (conde
       ((numbero expr) (l== expr val))

       ((symbolo expr) (lookupo #t expr env val))

       ((fresh (rator rands)
          (== `(,rator . ,rands) expr)
          (conde
            ((conde
               ((varo rator))
               ((non-varo rator) (not-ground-spineo rands)))
             (later `(u-eval-expo ,(expand expr) ,(expand env) ,(expand val))))
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
                  (l== val v)))
               ((fresh (rep x body)
                  (== `(lambda ,x ,body) expr)
                  (conde
                    ((not-ground-paramso x)
                     (later `(u-eval-expo ,(expand expr) ,(expand env) ,(expand val))))
                    ((ground-paramso x)
                     (l== `(closure ,rep) val)
                     (conde
                       ;; Variadic
                       ((symbolo x))
                       ;; Multi-argument
                       ((list-of-symbolso x)))
                     (not-in-envo 'lambda env)
                     (lreify-call rep ((eval-apply-staged eval-apply-dyn) (x body env) (_ _)))))))
               ((fresh (proc a*)
                  (project (rator)
                    (cond ((symbol? rator)
                           (fresh ()
                             (lookupo #f rator env proc)
                             (project (proc)
                               (cond ((and (pair? proc) (eq? (car proc) 'prim))
                                      (fresh (prim-id)
                                        (== proc `(prim . ,prim-id))
                                        ;;(non-varo prim-id)
                                        (eval-primo prim-id a* val)
                                        (eval-listo rands env a*)))
                                     (else
                                      (fresh ()
                                        (eval-listo rands env a*)
                                        (later `(callo ,(expand proc) ,(expand val) ,(expand a*)))))))))
                          (else
                           (fresh ()
                             (eval-expo rator env proc)
                             (eval-listo rands env a*)
                             (later `(callo ,(expand proc) ,(expand val) ,(expand a*)))))))))
               ((handle-matcho expr env val))
               ((fresh (letrec-body f x e rep env^)
                  (== `(letrec^ ((,f (lambda (,x) ,e))) ,letrec-body) expr)
                  (lreify-call rep ((eval-apply-rec-staged eval-apply-rec-dyn) (f x e env) (_ _)))
                  (== env^ `((,f . (val . (rec-closure ,rep))) . ,env))
                  (eval-expo letrec-body env^ val)))
               ((fresh (bindings* letrec-body out-bindings* env^)
                  (== `(letrec ,bindings*
                         ,letrec-body)
                      expr)
                  (conde
                    ((not-letrec-bindings-checko bindings*)
                     (later `(u-eval-expo ,(expand expr) ,(expand env) ,(expand val))))
                    ((letrec-bindings-checko bindings*)
                     (letrec-bindings-evalo bindings* out-bindings* env env^ env^)
                     (not-in-envo 'letrec env)
                     (fresh (c-letrec-body)
                       (later-scope
                        (eval-expo letrec-body env^ val)
                        c-letrec-body)
                       (later `(letrec ,out-bindings*
                                (fresh () . ,c-letrec-body))))))))
               ((prim-expo expr env val))
               )))))

       ((boolean-primo expr env val))

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
       (== e `(,p-name . (staged-rec (lambda ,x ,body) ,(unexpand p-name))))
       (== (cons o os) out-bindings*)
       (letrec-bindings-evalo bs os env envt es)
       (conde
            ((symbolo x)
             (== x^ (unexpand x))
             (== `((,x . (val . ,x^)) . ,envt) res))
            ((list-of-symbolso x)
             (make-list-of-symso x x^)
             (ext-env*o x x^ envt res)))
       (later-scope
        (eval-expo body res out)
        c-body)
       (== o `(,p-name (lambda ,x (lambda (,out) (fresh () . ,c-body)))))))))

(define empty-env '())

(define (lookupo stage? x env t)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (conde
         ((fresh (v) (== `(val . ,v) b) ((if stage? l== ==) t v)))
         ((fresh (lam-expr code-expr)
            (== `(staged-rec ,lam-expr ,code-expr) b)
            ((if stage? l== ==) `(call ,(unexpand x)) t)))))
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
       (eval-expo a env v-a)
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
    [(== prim-id 'list)
     (l== a* val)]
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
       (later `(conde
                ((=/= #f ,(expand b)) (== #f ,(expand val)))
                ((== #f ,(expand b)) (== #t ,(expand val))))))]
    [(== prim-id 'equal?)
     (fresh (v1 v2)
       (l== `(,v1 ,v2) a*)
       (later `(conde
                ((== ,(expand v1) ,(expand v2)) (== #t ,(expand val)))
                ((=/= ,(expand v1) ,(expand v2)) (== #f ,(expand val))))))]
    [(== prim-id 'symbol?)
     (fresh (v)
       (l== `(,v) a*)
       (later `(conde
                ((symbolo ,(expand v)) (== #t ,(expand val)))
                ((numbero ,(expand v)) (== #f ,(expand val)))
               ((fresh (a d)
                  (== `(,a . ,d) ,(expand v))
                  (== #f ,(expand val))))
               ((booleano ,(expand v)) (== #f ,(expand val))))))]
    [(== prim-id 'number?)
     (fresh (v)
       (l== `(,v) a*)
       (later `(conde
                ((numbero ,(expand v)) (== #t ,(expand val)))
                ((symbolo ,(expand v)) (== #f ,(expand val)))
                ((fresh (a d)
                   (== `(,a . ,d) ,(expand v))
                   (== #f ,(expand val))))
                ((booleano ,(expand v)) (== #f ,(expand val))))))]
    [(== prim-id 'pair?)
     (fresh (v)
       (l== `(,v) a*)
       (later `(conde
                ((symbolo ,(expand v)) (== #f ,(expand val)))
                ((numbero ,(expand v)) (== #f ,(expand val)))
                ((booleano ,(expand v)) (== #f ,(expand val)))
               ((fresh (a d)
                  (== `(,a . ,d) ,(expand v))
                  (== #t ,(expand val))
                  (not-tago a)))
               ((fresh (a d)
                  (== `(,a . ,d) ,(expand v))
                  (== #f ,(expand val))
                  (conde
                    ((== a 'closure))
                    ((== a 'prim))
                    ((== a 'call))
                    ((== a 'dynamic))
                    ((== a 'call-code))))))))]
    [(== prim-id 'null?)
     (fresh (v)
       (l== `(,v) a*)
       (later `(conde
                ((== '() ,(expand v)) (== #t ,(expand val)))
                ((=/= '() ,(expand v)) (== #f ,(expand val))))))]))

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
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v c)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (eval-expo e1 env v)
       (later-scope
        (ando `(,e2 . ,e-rest) env val)
        c)
       (later `(conde
                ((== #f ,(expand v))
                 (== #f ,(expand val)))
                ((=/= #f ,(expand v))
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
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v c)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (eval-expo e1 env v)
       (later-scope
        (oro `(,e2 . ,e-rest) env val)
        c)
       (later `(conde
                ((=/= #f ,(expand v))
                 (== ,(expand v) ,(expand val)))
                ((== #f ,(expand v))
                 . ,c)))))))

(define (if-primo expr env val)
  (fresh (e1 e2 e3 t c2 c3)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    (eval-expo e1 env t)
    (later-scope (eval-expo e2 env val) c2)
    (later-scope (eval-expo e3 env val) c3)
    (later `(conde
             ((=/= #f ,(expand t)) . ,c2)
             ((== #f ,(expand t)) . ,c3)))))

(define (choice-primo expr env val)
  (fresh (e2 e3 c2 c3)
    (== `(choice ,e2 ,e3) expr)
    (not-in-envo 'choice env)
    (later-scope (eval-expo e2 env val) c2)
    (later-scope (eval-expo e3 env val) c3)
    (later `(conde
            ,c2
            ,c3))))

(define initial-env `((list . (val . (prim . list)))
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
         (later `(u-eval-expo ,(expand expr) ,(expand env) ,(expand val))))
        ((match-checko (cons clause clauses))
         (fresh (mval)
           (not-in-envo 'match env)
           (eval-expo against-expr env mval)
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
    ; a match fails if no clause matches; in
    ; unstaged this happens when reaching
    ; match-clauses with an empty list of clauses.
    ; in staged, defer to runtime.
    ((== clauses '())
     (later 'fail))
    ((fresh (p result-expr d penv c-yes c-no)
       (== `((,p ,result-expr) . ,d) clauses)
       (later-scope
        (fresh (env^)
          (p-match p mval '() penv)
          (regular-env-appendo penv env env^)
          (eval-expo result-expr env^ val))
        c-yes)
       (later-scope
        (fresh ()
          (p-no-match p mval '() penv)
          (match-clauses mval d env val))
        c-no)
       (later `(conde ,c-yes ,c-no))))))

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
  (conde
    ; a variable pattern cannot fail when it is
    ; the first occurence of the name. unstaged
    ; fails by failure of the lookupo below; in
    ; staged we need to defer this failure to
    ; runtime.
    ((symbolo var)
     (not-in-envo var penv)
     (later 'fail))
    ((fresh (val)
       (symbolo var)
       (l=/= mval val)
       (== penv penv-out)
       (lookupo #t var penv val)))))

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
         (later `(symbolo ,(expand mval))))
        ((== 'number? pred)
         (later `(numbero ,(expand mval)))))
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
            (later-scope
             (fresh ()
               (later `(not-symbolo ,(expand mval))))
             z1)
            (later-scope
             (fresh ()
               (later `(symbolo ,(expand mval)))
               (var-p-no-match var mval penv penv-out))
             z2)
            (later `(conde ,z1 ,z2))))
         ((== 'number? pred)
          (fresh (z1 z2)
            (later-scope
             (fresh ()
               (later `(not-numbero ,(expand mval))))
             z1)
            (later-scope
             (fresh ()
               (later `(numbero ,(expand mval)))
               (var-p-no-match var mval penv penv-out))
             z2)
            (later `(conde ,z1 ,z2)))))))
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
      ((fresh (a d)
         (== `(,a . ,d) quasi-p)
         (=/= 'unquote a)
         (fresh (z1 z2)
           (later-scope
             (fresh ()
               (== penv penv-out)
               (later `(literalo ,(expand mval))))
             z1)
           (later-scope
             (fresh (penv^ v1 v2)
               (l== `(,v1 . ,v2) mval)
               (fresh (z3 z4)
                 (later-scope
                   (fresh ()
                     (quasi-p-no-match a v1 penv penv-out))
                   z3)
                 (later-scope
                   (fresh ()
                     (quasi-p-match a v1 penv penv^)
                     (quasi-p-no-match d v2 penv^ penv-out))
                   z4)
                 (later `(conde ,z3 ,z4))))
             z2)
           (later `(conde ,z1 ,z2)))
         )))))
