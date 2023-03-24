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
    (lapp u-eval-expo expr env val)
    ([] [(numbero expr)] [(l== expr val)])
    ([] [(symbolo expr)] [(fresh (env-v) (lookupo expr env env-v) (l== env-v val))])
    ([v]
     [(== `(quote ,v) expr)
      (absent-tago v)
      (not-in-envo 'quote env)]
     [(l== val v)])
    ([x body]
     [(== `(lambda ,x ,body) expr)
      (not-in-envo 'lambda env)
      (conde
        ((symbolo x))
        ((list-of-symbolso x)))]
     [(fresh (rep)
        (l== `(closure ,rep) val)
        ;; could imagine the following line as (l== (eval-apply x body env) rep)
        (lreify-call rep ((eval-apply-staged eval-apply-dyn) (x body env) (_ _))))])
     ;; statically-recognizable primitive application
    ([rator rands a* prim]
     [(== `(,rator . ,rands) expr)
      (symbolo rator)
      (lookupo rator env `(prim . ,prim))]
     [(fresh (proc)
        (eval-primo prim a* val)
        (eval-listo rands env a*))])
    ;; general application
    ([rator rands a*]
     [(== `(,rator . ,rands) expr)
      (conde
        ((symbolo rator)
         (fresh (proc p tag)
           (lookupo rator env proc)
           (== `(,tag . ,p) proc)
           (=/= 'prim tag)))
        ((fresh (a d) (== (cons a d) rator))))]
     [(fresh (proc)
        (eval-expo rator env proc)
        (eval-listo rands env a*)
        (lapp callo proc val a*))])

    
    ;; match
    ([against-expr clauses]
     [(== `(match ,against-expr . ,clauses) expr)
      (not-in-envo 'match env)]
     [
      (fresh (mval)
        (eval-expo against-expr env mval)
        (match-clauses mval clauses env val))
      ])

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
        (lconde
         ((l=/= #f t) (eval-expo e2 env val))
         ((l== #f t) (eval-expo e3 env val))))])
    ;; boolean-primo
    ([] [(== #t expr)] [(l== #t val)])
    ([] [(== #f expr)] [(l== #f val)]))
  )

(define empty-env '())

(define (lookupo x env v)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (condg
      (lapp u-lookupo x env v)
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
    (lapp u-eval-listo expr env val)
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
  (condg
   (lapp u-eval-primo prim-id a* val)
   ([] [(== prim-id 'list)]
    [(l== a* val)])
   ([] [(== prim-id 'cons)]
    [(fresh (a d)
        (l== `(,a ,d) a*)
        (l== `(,a . ,d) val))])
   ([] [(== prim-id 'car)]
    [(fresh (d)
          (l== `((,val . ,d)) a*)
          (not-tago val))])
   ([] [(== prim-id 'cdr)]
    [(fresh (a)
       (l== `((,a . ,val)) a*)
       (not-tago a))])
   ([] [(== prim-id 'not)]
    [(fresh (b)
       (l== `(,b) a*)
       (lconde
        ((l=/= #f b) (l== #f val))
        ((l== #f b) (l== #t val))))])
   ([] [(== prim-id 'equal?)]
    [(fresh (v1 v2)
       (l== `(,v1 ,v2) a*)
       (lconde
        [(l== v1 v2) (l== #t val)]
        [(l=/= v1 v2) (l== #f val)]))])
   ([] [(== prim-id 'symbol?)]
    [(fresh (v)
       (l== `(,v) a*)
       (lconde
        ((lsymbolo v) (l== #t val))
        ((lnumbero v) (l== #f val))
        ((fresh (a d)
           (l== `(,a . ,d) v)
           (l== #f val)))
        ((lapp booleano v) (l== #f val))))])
   ([] [(== prim-id 'number?)]
    [(fresh (v)
       (l== `(,v) a*)
       (lconde
         ((lnumbero v) (l== #t val))
         ((lsymbolo v) (l== #f val))
         ((fresh (a d)
            (l== `(,a . ,d) v)
            (l== #f val)))
         ((lapp booleano v) (l== #f val))))])
   ([] [(== prim-id 'pair?)]
    [(fresh (v)
       (l== `(,v) a*)
       (lconde
         ((lsymbolo v) (l== #f val))
         ((lnumbero v) (l== #f val))
         ((lapp booleano v) (l== #f val))
         ((fresh (a d)
            (l== `(,a . ,d) v)
            (l== #t val)
            (lapp not-tago a)))
         ((fresh (a d)
            (l== `(,a . ,d) v)
            (l== #f val)
            (lapp pos-tago a)))))])
   ([] [(== prim-id 'null?)]
    [(fresh (v)
       (l== `(,v) a*)
       (lconde
         ((l== '() v) (l== #t val))
         ((l=/= '() v) (l== #f val))))])))

(define (ando e* env val)
  (condg
    (lapp u-ando e* env val)
    ([] [(== '() e*)] [(l== #t val)])
    ([e] [(== `(,e) e*)] [(eval-expo e env val)])
    ([e1 e2 e-rest]
     [(== `(,e1 ,e2 . ,e-rest) e*)]
     [(fresh (v c)
        (eval-expo e1 env v)
        (lconde
         ((l== #f v)
          (l== #f val))
         ((=/= #f v)
          (ando `(,e2 . ,e-rest) env val))))])))

(define (oro e* env val)
  (condg
    (lapp u-oro e* env val)
    ([] [(== '() e*)] [(l== #f val)])
    ([e] [(== `(,e) e*)] [(eval-expo e env val)])
    ([e1 e2 e-rest]
     [(== `(,e1 ,e2 . ,e-rest) e*)]
     [(fresh (v c)
        (eval-expo e1 env v)
        (lconde
         ((l=/= #f v)
          (l== v val))
         ((l== #f v)
          (oro `(,e2 . ,e-rest) env val))))])))

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
  (condg
   (lapp u-match-clauses mval clauses env val)
    ;; a match fails if no clause matches; in
    ;; unstaged this happens when reaching
    ;; match-clauses with an empty list of clauses.
    ;; in staged, defer to runtime.
   ([]  [(== clauses '())] [lfail])
   ([p result-expr d penv c-yes c-no]
    [(== `((,p ,result-expr) . ,d) clauses)]
    [(lconde
       [(fresh (env^)
          (p-match p mval '() penv)
          (regular-env-appendo penv env env^)
          (eval-expo result-expr env^ val))]
       [(p-no-match p mval '() penv)
        (match-clauses mval d env val)])])))

(define (var-p-match var mval penv penv-out)
  (fresh (val)
    (lapp not-tago mval)
    (l== mval val)
    (var-p-match-extend var val penv penv-out)))

(define (var-p-match-extend var val penv penv-out)
  (condg
   (lapp u-var-p-match-extend var val penv penv-out)
   ([env-v] [(u-lookupo var penv env-v)]
    [(== penv penv-out)
     (l== env-v val)])
   ([]
    [(not-in-envo var penv)]
    [(== `((,var . (val . ,val)) . ,penv) penv-out)])))

(define (var-p-no-match var mval penv penv-out)
  (condg
   (lapp u-var-p-no-match var mval penv penv-out)
    ;; a variable pattern cannot fail when it is
    ;; the first occurence of the name. unstaged
    ;; fails by failure of the lookupo below; in
    ;; staged we need to defer this failure to
    ;; runtime.
   ([] [(not-in-envo var penv)]
    [lfail])
   ([env-v]
    [(== penv penv-out)
     (u-lookupo var penv env-v)]
    [(l=/= mval env-v)])))

(define (p-match p mval penv penv-out)
  (condg
   (lapp u-p-match p mval penv penv-out)
   ([] [(self-eval-literalo p)]
    [(l== p mval)
     (== penv penv-out)])
   ([] [(symbolo p)] [(var-p-match p mval penv penv-out)])
   ([var pred]
    [(== `(? ,pred ,var) p)]
    [(pred-match pred mval)
     (var-p-match var mval penv penv-out)])
   ([quasi-p] [(== (list 'quasiquote quasi-p) p)]
    [(quasi-p-match quasi-p mval penv penv-out)])))

(define (pred-match pred mval)
  (condg
   (lapp u-pred-match pred mval)
   ([] [(== 'symbol? pred)] [(lsymbolo mval)])
   ([] [(== 'number? pred)] [(lnumbero mval)])))

(define (p-no-match p mval penv penv-out)
  (condg
   (lapp u-p-no-match p mval penv penv-out)
   ([] [(self-eval-literalo p)]
    [(l=/= p mval)
     (== penv penv-out)])
   ([] [(symbolo p)] [(var-p-no-match p mval penv penv-out)])
   ([var pred] [(== `(? ,pred ,var) p)]
    [(== penv penv-out)
     (symbolo var)
     (pred-no-match pred var mval penv penv-out)])
   ([quasi-p] [(== (list 'quasiquote quasi-p) p)]
    [(quasi-p-no-match quasi-p mval penv penv-out)])))

(define (pred-no-match pred var mval penv penv-out)
  (condg
   (lapp u-pred-no-match pred var mval penv penv-out)
   ([] [(== 'symbol? pred)]
    [(lconde
       [(lapp not-symbolo mval)]
       [(lsymbolo mval)
        (var-p-no-match var mval penv penv-out)])])
   ([] [(== 'number? pred)]
    [(lconde
       [(lapp not-numbero mval)]
       [(lnumbero mval)
        (var-p-no-match var mval penv penv-out)])])))

(define (quasi-p-match quasi-p mval penv penv-out)
  (condg
   (lapp u-quasi-p-match quasi-p mval penv penv-out)
   ([] [(literalo quasi-p)]
    [(l== quasi-p mval)
     (== penv penv-out)])
   ([p] [(== (list 'unquote p) quasi-p)]
    [(p-match p mval penv penv-out)])
   ([a d v1 v2 penv^]
    [(== `(,a . ,d) quasi-p)
     (=/= 'unquote a)]
    [(l== `(,v1 . ,v2) mval)
     (quasi-p-match a v1 penv penv^)
     (quasi-p-match d v2 penv^ penv-out)])))

(define (quasi-p-no-match quasi-p mval penv penv-out)
  (condg
   (lapp u-quasi-p-no-match quasi-p mval penv penv-out)
   ([] [(literalo quasi-p)]
    [(l=/= quasi-p mval)
     (== penv penv-out)])
   ([p] [(== (list 'unquote p) quasi-p)]
    [(lapp not-tago mval) ;; TODO: why do we need this?
     (p-no-match p mval penv penv-out)])
   ([a d]
    [(== `(,a . ,d) quasi-p)
     (=/= 'unquote a)]
    [(lconde
       [(== penv penv-out)
        (lapp literalo mval)]
       [(fresh (penv^ v1 v2)
          (l== `(,v1 . ,v2) mval)
          (lconde
           [(quasi-p-no-match a v1 penv penv-out)]
           [(quasi-p-match a v1 penv penv^)
            (quasi-p-no-match d v2 penv^ penv-out)]))])])))

(define (evalo-staged expr val)
  (eval-expo expr initial-env val))

(define (evalo-unstaged expr val)
  (u-eval-expo expr initial-env val))
