(define (eval-apply-rec-staged rep f x* e env a* res)
  (fresh (env^ env-self)
    ;; TODO
    ;; (lreify-call rep ((eval-apply-rec-staged eval-apply-rec-dyn) (f x* e env) (_ _)))
    (== env-self `((,f . (val . (rec-closure ,rep))) . ,env))
    (conde
      ((symbolo x*)
       (== env^ `((,x* . (val . ,a*)) . ,env-self)))
      ((list-of-symbolso x*)
       (ext-env*o x* a* env-self env^)))
    (eval-expo e env^ res)))

(define (eval-apply-rec-dyn f x* e env a* res)
  (fresh (rep env^ env-self)
    (reify-call rep ((eval-apply-rec-staged eval-apply-rec-dyn) (f x* e env) (_ _)))
    (== env-self `((,f . (val . (rec-closure ,rep))) . ,env))
    (conde
      ((symbolo x*)
       (== env^ `((,x* . (val . ,a*)) . ,env-self)))
      ((u-ext-env*o x* a* env-self env^)))
    (u-eval-expo e env^ res)))

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

(define (booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

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

(define (pos-tago v)
  (conde
    ((== v 'rec-closure))
    ((== v 'closure))
    ((== v 'prim))))

(define (not-tago v)
  (fresh ()
    (=/= 'rec-closure v)
    (=/= 'closure v)
    (=/= 'prim v)))

(define (absent-tago v)
  (fresh ()
    (absento 'rec-closure v)
    (absento 'closure v)
    (absento 'prim v)))

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
       ;; maybe we can write something closer to this:
       ;; (apply-reified rep eval-apply a* val)
       (apply-reified rep ((eval-apply-staged eval-apply-dyn) (_ _ _) (a* val)))))
    ((fresh (rep)
       (== proc `(rec-closure ,rep))
       (apply-reified rep ((eval-apply-rec-staged eval-apply-rec-dyn) (_ _ _ _) (a* val)))))
    ((fresh (prim-id)
       (== proc `(prim . ,prim-id))
       (u-eval-primo prim-id a* val)))))

(define (eval-expo expr env val)
  (condg
    (later `(u-eval-expo ,(expand expr) ,(expand env) ,(expand val)))
    ([] [(numbero expr)] [(l== expr val)])
    ([] [(symbolo expr)] [(fresh (env-v) (lookupo expr env env-v) (l== env-v val))])
    ([v]
     [(== `(quote ,v) expr)
      (absent-tago v)
      (not-in-envo 'quote env)]
     [(l== val v)])
    ([x body]
     [(== `(lambda ,x ,body) expr)
      (not-in-envo 'lambda env)]
     [(project (x)
        (if (or (symbol? x) (and (list? x) (andmap symbol? x)))
            (fresh (rep)
              (l== `(closure ,rep) val)
              ;; could imagine the following line as (l== (eval-apply x body env) rep)
              (lreify-call rep ((eval-apply-staged eval-apply-dyn) (x body env) (_ _))))
            (later `(u-eval-expo ,(expand expr) ,(expand env) ,(expand val)))))])
    ;; statically-recognizable primitive application
    ([rator rands a* prim]
     [(== `(,rator . ,rands) expr)
      (non-varo rator)
      (symbolo rator)
      (lookupo rator env `(prim . ,prim))
      (non-varo prim)]
     [(fresh (proc)
        (eval-primo prim a* val)
        (eval-listo rands env a*))])
    ;; general application
    ([rator rands a*]
     [(== `(,rator . ,rands) expr)
      (conde
        ((varo rator))
        ((non-varo rator)
         (symbolo rator)
         (fresh (proc p tag)
           (lookupo rator env proc)
           (conde
             ((varo proc))
             ((non-varo proc)
              (== `(,tag . ,p) proc)
              (=/= 'prim tag)))))
        ((non-varo rator)
         (fresh (a d) (== (cons a d) rator))))]
     [(fresh (proc)
        (eval-expo rator env proc)
        (eval-listo rands env a*)
        (later `(callo ,(expand proc) ,(expand val) ,(expand a*))))])

    
    ;; match
    ([against-expr clauses]
     [(== `(match ,against-expr . ,clauses) expr)
      (not-in-envo 'match env)]
     [(fresh (mval)
        (eval-expo against-expr env mval)
        (match-clauses mval clauses env val))])

    ;; letrec
    ([letrec-body f x e]
     [(== `(letrec ((,f (lambda ,x ,e))) ,letrec-body) expr)
      (not-in-envo 'letrec env)]
     [(fresh (rep env^)
        (== env^ `((,f . (val . (rec-closure ,rep))) . ,env))
        (lreify-call rep ((eval-apply-rec-staged eval-apply-rec-dyn) (f x e env) (_ _)))
        (eval-expo letrec-body env^ val))])
    ;; and-primo
    ([e*] [(== `(and . ,e*) expr) (not-in-envo 'and env)] [(ando e* env val)])
    ;; or-primo
    ([e*] [(== `(or . ,e*) expr) (not-in-envo 'or env)] [(oro e* env val)])
    ;; if-primo
    ([e1 e2 e3]
     [(== `(if ,e1 ,e2 ,e3) expr) (not-in-envo 'if env)]
     [(fresh (t c2 c3)
        (eval-expo e1 env t)
        (later-scope (eval-expo e2 env val) c2)
        (later-scope (eval-expo e3 env val) c3)
        (later `(conde
                  ((=/= #f ,(expand t)) . ,c2)
                  ((== #f ,(expand t)) . ,c3))))])
    ;; boolean-primo
    ([] [(== #t expr)] [(l== #t val)])
    ([] [(== #f expr)] [(l== #f val)]))
  )

(define empty-env '())

(define (lookupo x env v)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (condg
      (later `(u-lookupo ,(expand x) ,(expand env) ,(expand v)))
      ([] [(== x y)] [(== `(val . ,v) b)])
      ([] [(=/= x y)] [(lookupo x rest v)]))))

(define (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))

(define (old-eval-listo expr env val)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expo a env v-a)
       (eval-listo d env v-d)))))

(define (eval-listo expr env val)
  (condg
    (later `(u-eval-listo ,(expand expr) ,(expand env) ,(expand val)))
    ([] [(== '() expr)] [(== '() val)])
    ([a d v-a v-d]
     [(== `(,a . ,d) expr)
      (== `(,v-a . ,v-d) val)]
     [(eval-expo a env v-a)
      (eval-listo d env v-d)])))

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
                    (pos-tago a))))))]
    [(== prim-id 'null?)
     (fresh (v)
       (l== `(,v) a*)
       (later `(conde
                 ((== '() ,(expand v)) (== #t ,(expand val)))
                 ((=/= '() ,(expand v)) (== #f ,(expand val))))))]))

(define (ando e* env val)
  (condg
    (later `(u-ando ,(expand e*) ,(expand env) ,(expand val)))
    ([] [(== '() e*)] [(l== #t val)])
    ([e] [(== `(,e) e*)] [(eval-expo e env val)])
    ([e1 e2 e-rest]
     [(== `(,e1 ,e2 . ,e-rest) e*)]
     [(fresh (v c)
        (eval-expo e1 env v)
        (later-scope
         (ando `(,e2 . ,e-rest) env val)
         c)
        (later `(conde
                  ((== #f ,(expand v))
                   (== #f ,(expand val)))
                  ((=/= #f ,(expand v))
                   . ,c))))])))

(define (oro e* env val)
  (condg
    (later `(u-oro ,(expand e*) ,(expand env) ,(expand val)))
    ([] [(== '() e*)] [(l== #f val)])
    ([e] [(== `(,e) e*)] [(eval-expo e env val)])
    ([e1 e2 e-rest]
     [(== `(,e1 ,e2 . ,e-rest) e*)]
     [(fresh (v c)
        (eval-expo e1 env v)
        (later-scope
         (oro `(,e2 . ,e-rest) env val)
         c)
        (later `(conde
                  ((=/= #f ,(expand v))
                   (== ,(expand v) ,(expand val)))
                  ((== #f ,(expand v))
                   . ,c))))])))

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

(define (regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

(define (match-clauses mval clauses env val)
  (conde
    ;; a match fails if no clause matches; in
    ;; unstaged this happens when reaching
    ;; match-clauses with an empty list of clauses.
    ;; in staged, defer to runtime.
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
       (fresh (env-v)
         (lookupo var penv env-v)
         (l== env-v val)))
      ((== `((,var . (val . ,val)) . ,penv) penv-out)
       (not-in-envo var penv)))))

(define (var-p-no-match var mval penv penv-out)
  (conde
    ;; a variable pattern cannot fail when it is
    ;; the first occurence of the name. unstaged
    ;; fails by failure of the lookupo below; in
    ;; staged we need to defer this failure to
    ;; runtime.
    ((symbolo var)
     (not-in-envo var penv)
     (later 'fail))
    ((fresh (val)
       (symbolo var)
       (l=/= mval val)
       (== penv penv-out)
       (fresh (env-v)
         (lookupo var penv env-v)
         (l== env-v val))))))

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
