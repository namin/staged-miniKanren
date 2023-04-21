(defrel/generator (absent-tago/gen v)
  (absento 'rec-closure v)
  (absento 'closure v)
  (absento 'prim v))

(defrel/generator (not-tago/gen v)
  (=/= 'rec-closure v)
  (=/= 'closure v)
  (=/= 'prim v))

(defrel/generator (booleano/gen t)
  (conde
    ((== #f t))
    ((== #t t))))

(defrel/generator (eval-apply-rec-staged rep f x* e env a* res)
  (fresh (env^ env-self)
    ;; TODO
    ;; (lreify-call rep ((eval-apply-rec-staged eval-apply-rec-dyn) (f x* e env) (_ _)))
    (== env-self `((,f . (val . (rec-closure ,rep))) . ,env))
    ;; TODO: should be condg-ified
    (conde
      ((symbolo x*)
       (== env^ `((,x* . (val . ,a*)) . ,env-self)))
      ((list-of-symbolso x*)
       (ext-env*o x* a* env-self env^)))
    (eval-expo e env^ res)))

(defrel-partial (eval-apply-rec [f x* e env] [a* res])
  #:generator eval-apply-rec-staged
  (fresh (rep env^ env-self)
    (== rep (partial-apply eval-apply-rec f x* e env))
    (== env-self `((,f . (val . (rec-closure ,rep))) . ,env))
    (conde
      ((symbolo x*)
       (== env^ `((,x* . (val . ,a*)) . ,env-self)))
      ((u-ext-env*o x* a* env-self env^)))
    (u-eval-expo e env^ res)))

(defrel/generator (eval-apply-staged rep x* body env a* val)
  (fresh (env^)
    ;; TODO: should be condg-ified
    (conde
      ((symbolo x*)
       (== `((,x* . (val . ,a*)) . ,env) env^))
      ((list-of-symbolso x*)
       (ext-env*o x* a* env env^)))
    (eval-expo body env^ val)))

(defrel-partial (eval-apply [x* body env] [a* val])
  #:generator eval-apply-staged
  (fresh (env^)
    (conde
      ((symbolo x*)
       (== `((,x* . (val . ,a*)) . ,env) env^))
      ((u-ext-env*o x* a* env env^)))
    (u-eval-expo body env^ val)))

(defrel (callo proc val a*)
  (conde
    ((fresh (rep)
       (== proc `(closure ,rep))
       (apply-partial rep eval-apply a* val)))
    ((fresh (rep)
       (== proc `(rec-closure ,rep))
       (apply-partial rep eval-apply-rec a* val)))
    ((fresh (prim-id)
       (== proc `(prim . ,prim-id))
       (u-eval-primo prim-id a* val)))))

(defrel/generator (eval-appo rator rands env val)
  (condg
   #:fallback
   (fresh (proc a*)
     (eval-expo rator env proc)
     (eval-listo rands env a*)
     (later (callo proc val a*)))
   
   ;; statically-recognizable primitive application
   ([prim]
    [(symbolo rator)
     (lookupo rator env `(prim . ,prim))]
    [(fresh (proc a*)
       (eval-primo prim a* val)
       (eval-listo rands env a*))])
    
   ;; general application
   ([]
    [(conde
      ((symbolo rator)
       (fresh (p tag)
         (lookupo rator env `(,tag . ,p))
         (=/= 'prim tag)))
      ((fresh (a d) (== (cons a d) rator))))]
    [(fresh (proc a*)
       (eval-expo rator env proc)
       (eval-listo rands env a*)
       (later (callo proc val a*)))])))

(defrel/generator (eval-expo expr env val)
  (condg
    #:fallback (later (u-eval-expo expr env val))

    ;; number literal
    ([] [(numbero expr)] [(later (== expr val))])

    ;; variable reference
    ([]
     [(symbolo expr)]
     [(fresh (env-v)
        (lookupo expr env env-v)
        (later (== env-v val)))])

    ;; quote
    ([v]
     [(== `(quote ,v) expr)
      (absent-tago/gen v)
      (not-in-envo 'quote env)]
     [(later (== val v))])

    ;; lambda
    ([x body]
     [(== `(lambda ,x ,body) expr)
      (not-in-envo 'lambda env)
      (conde
        ((symbolo x))
        ((list-of-symbolso x)))]
     [(later (fresh (rep)
               (== `(closure ,rep) val)
               (== rep (partial-apply eval-apply x body env))))])

    ;; application
    ([rator rands a* rator-v]
     [(== `(,rator . ,rands) expr)
      (conde
       ((symbolo rator)
        (lookupo rator env rator-v))
       ((fresh (a d) (== (cons a d) rator))))]
     [(eval-appo rator rands env val)])
    
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
        (later (== rep (partial-apply eval-apply-rec f x e env)))
        (eval-expo letrec-body env^ val))])
    
    ;; and
    ([e*] [(== `(and . ,e*) expr) (not-in-envo 'and env)] [(ando e* env val)])
    
    ;; or
    ([e*] [(== `(or . ,e*) expr) (not-in-envo 'or env)] [(oro e* env val)])
    
    ;; if
    ([e1 e2 e3]
     [(== `(if ,e1 ,e2 ,e3) expr) (not-in-envo 'if env)]
     [(fresh (t c2 c3)
        (eval-expo e1 env t)
        (later (conde
                 ((=/= #f t) (now (eval-expo e2 env val)))
                 ((== #f t) (now (eval-expo e3 env val))))))])
    
    ;; boolean literals
    ([] [(== #t expr)] [(later (== #t val))])
    ([] [(== #f expr)] [(later (== #f val))]))
  )

(define empty-env '())

(defrel/generator (lookupo x env v)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (condg
      #:fallback (later (u-lookupo x env v))
      ([] [(== x y)] [(== `(val . ,v) b)])
      ([] [(=/= x y)] [(lookupo x rest v)]))))

(defrel/generator (match-lookupo/gen x env t)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (== `(val . ,t) b))
      ((=/= x y)
       (match-lookupo/gen x rest t)))))

(defrel/generator (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))

(defrel/generator (eval-listo expr env val)
  (condg
    #:fallback (later (u-eval-listo expr env val))
    ([] [(== '() expr)] [(== '() val)])
    ([a d v-a v-d]
     [(== `(,a . ,d) expr)
      (== `(,v-a . ,v-d) val)]
     [(eval-expo a env v-a)
      (eval-listo d env v-d)])))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(defrel/generator (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(defrel/generator (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((,x . (val . ,a)) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

(defrel/generator (eval-primo prim-id a* val)
  (condg
   #:fallback (later (u-eval-primo prim-id a* val))
   ([]
    [(== prim-id 'list)]
    [(later (== a* val))])
   ([]
    [(== prim-id 'cons)]
    [(later (fresh (a d)
              (== `(,a ,d) a*)
              (== `(,a . ,d) val)))])
   ([]
    [(== prim-id 'car)]
    [(later (fresh (d)
              (== `((,val . ,d)) a*)
              (not-tago val)))])
   ([]
    [(== prim-id 'cdr)]
    [(later
      (fresh (a)
        (== `((,a . ,val)) a*)
        (not-tago a)))])
   ([]
    [(== prim-id 'not)]
    [(later
      (fresh (b)
        (== `(,b) a*)
        (conde
          ((=/= #f b) (== #f val))
          ((== #f b) (== #t val)))))])
   ([]
    [(== prim-id 'equal?)]
    [(later (fresh (v1 v2)
              (== `(,v1 ,v2) a*)
              (conde
                [(== v1 v2) (== #t val)]
                [(=/= v1 v2) (== #f val)])))])
   ([]
    [(== prim-id 'symbol?)]
    [(later (fresh (v)
              (== `(,v) a*)
              (conde
               ((symbolo v) (== #t val))
               ((numbero v) (== #f val))
               ((fresh (a d)
                  (== `(,a . ,d) v)
                  (== #f val)))
               ((booleano v) (== #f val)))))])
   ([]
    [(== prim-id 'number?)]
    [(later (fresh (v)
              (== `(,v) a*)
              (conde
               ((numbero v) (== #t val))
               ((symbolo v) (== #f val))
               ((fresh (a d)
                  (== `(,a . ,d) v)
                  (== #f val)))
               ((booleano v) (== #f val)))))])
   ([]
    [(== prim-id 'pair?)]
    [(later (fresh (v)
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
                  (pos-tago a))))))])
   ([]
    [(== prim-id 'null?)]
    [(later (fresh (v)
              (== `(,v) a*)
              (conde
               ((== '() v) (== #t val))
               ((=/= '() v) (== #f val)))))])))

(defrel/generator (ando e* env val)
  (condg
    #:fallback (later (u-ando e* env val))
    ([] [(== '() e*)] [(later (== #t val))])
    ([e] [(== `(,e) e*)] [(eval-expo e env val)])
    ([e1 e2 e-rest]
     [(== `(,e1 ,e2 . ,e-rest) e*)]
     [(fresh (v c)
        (eval-expo e1 env v)
        (later (conde
                 ((== #f v)
                  (== #f val))
                 ((=/= #f v)
                  (now (ando `(,e2 . ,e-rest) env val))))))])))

(defrel/generator (oro e* env val)
  (condg
    #:fallback (later (u-oro e* env val))
    ([] [(== '() e*)] [(later (== #f val))])
    ([e] [(== `(,e) e*)] [(eval-expo e env val)])
    ([e1 e2 e-rest]
     [(== `(,e1 ,e2 . ,e-rest) e*)]
     [(fresh (v c)
        (eval-expo e1 env v)
        (later (conde
                ((=/= #f v)
                 (== v val))
                ((== #f v)
                 (now (oro `(,e2 . ,e-rest) env val))))))])))

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

(defrel (not-symbolo t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(defrel (not-numbero t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((symbolo t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(defrel/generator (self-eval-literalo t)
  (conde
    ((numbero t))
    ((booleano/gen t))))

(defrel/generator (literalo t)
  (conde
    ((numbero t))
    ((symbolo t) (not-tago/gen t))
    ((booleano/gen t))
    ((== '() t))))

(defrel/generator (regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

(defrel/generator (match-clauses mval clauses env val)
  (condg
   #:fallback (later (u-match-clauses mval clauses env val))
    ;; a match fails if no clause matches; in
    ;; unstaged this happens when reaching
    ;; match-clauses with an empty list of clauses.
    ;; in staged, defer to runtime.
   ([]  [(== clauses '())] [(later fail)])
   ([p result-expr d penv c-yes c-no]
    [(== `((,p ,result-expr) . ,d) clauses)]
    [(later (conde
              [(now (fresh (env^)
                      (p-match p mval '() penv)
                      (regular-env-appendo penv env env^)
                      (eval-expo result-expr env^ val)))]
              [(now (p-no-match p mval '() penv))
               (now (match-clauses mval d env val))]))])))

(defrel/generator (var-p-match var mval penv penv-out)
  (fresh (val)
    (later (not-tago mval))
    (later (== mval val))
    (var-p-match-extend var val penv penv-out)))

(defrel/generator (var-p-match-extend var val penv penv-out)
  (condg
   #:fallback (later (u-var-p-match-extend var val penv penv-out))
   ([env-v]
    [(match-lookupo/gen var penv env-v)]
    [(== penv penv-out)
     (later (== env-v val))])
   ([]
    [(not-in-envo var penv)]
    [(== `((,var . (val . ,val)) . ,penv) penv-out)])))

(defrel/generator (var-p-no-match var mval penv penv-out)
  (condg
   #:fallback (later (u-var-p-no-match var mval penv penv-out))
    ;; a variable pattern cannot fail when it is
    ;; the first occurence of the name. unstaged
    ;; fails by failure of the lookupo below; in
    ;; staged we need to defer this failure to
    ;; runtime.
   ([]
    [(not-in-envo var penv)]
    [(later fail)])
   ([env-v]
    [(== penv penv-out)
     (match-lookupo/gen var penv env-v)]
    [(later (=/= mval env-v))])))

(defrel/generator (p-match p mval penv penv-out)
  (condg
   #:fallback (later (u-p-match p mval penv penv-out))
   ([]
    [(self-eval-literalo p)]
    [(later (== p mval))
     (== penv penv-out)])
   ([] [(symbolo p)] [(var-p-match p mval penv penv-out)])
   ([var pred]
    [(== `(? ,pred ,var) p)]
    [(pred-match pred mval)
     (var-p-match var mval penv penv-out)])
   ([quasi-p]
    [(== (list 'quasiquote quasi-p) p)]
    [(quasi-p-match quasi-p mval penv penv-out)])))

(defrel/generator (pred-match pred mval)
  (condg
   #:fallback (later (u-pred-match pred mval))
   ([] [(== 'symbol? pred)] [(later (symbolo mval))])
   ([] [(== 'number? pred)] [(later (numbero mval))])))

(defrel/generator (p-no-match p mval penv penv-out)
  (condg
   #:fallback (later (u-p-no-match p mval penv penv-out))
   ([]
    [(self-eval-literalo p)]
    [(later (=/= p mval))
     (== penv penv-out)])
   ([] [(symbolo p)] [(var-p-no-match p mval penv penv-out)])
   ([var pred]
    [(== `(? ,pred ,var) p)]
    [(== penv penv-out)
     (symbolo var)
     (pred-no-match pred var mval penv penv-out)])
   ([quasi-p] [(== (list 'quasiquote quasi-p) p)]
    [(quasi-p-no-match quasi-p mval penv penv-out)])))

(defrel/generator (pred-no-match pred var mval penv penv-out)
  (condg
   #:fallback (later (u-pred-no-match pred var mval penv penv-out))
   ([]
    [(== 'symbol? pred)]
    [(later (conde
              [(not-symbolo mval)]
              [(symbolo mval)
               (now (var-p-no-match var mval penv penv-out))]))])
   ([]
    [(== 'number? pred)]
    [(later (conde
              [(not-numbero mval)]
              [(numbero mval)
               (now (var-p-no-match var mval penv penv-out))]))])))

(defrel/generator (quasi-p-match quasi-p mval penv penv-out)
  (condg
   #:fallback (later (u-quasi-p-match quasi-p mval penv penv-out))
   ([]
    [(literalo quasi-p)]
    [(later (== quasi-p mval))
     (== penv penv-out)])
   ([p]
    [(== (list 'unquote p) quasi-p)]
    [(p-match p mval penv penv-out)])
   ([a d v1 v2 penv^]
    [(== `(,a . ,d) quasi-p)
     (=/= 'unquote a)]
    [(later (== `(,v1 . ,v2) mval))
     (quasi-p-match a v1 penv penv^)
     (quasi-p-match d v2 penv^ penv-out)])))

(defrel/generator (quasi-p-no-match quasi-p mval penv penv-out)
  (condg
   #:fallback (later (u-quasi-p-no-match quasi-p mval penv penv-out))
   ([]
    [(literalo quasi-p)]
    [(later (=/= quasi-p mval))
     (== penv penv-out)])
   ([p]
    [(== (list 'unquote p) quasi-p)]
    [(later (not-tago mval)) ;; TODO: why do we need this?
     (p-no-match p mval penv penv-out)])
   ([a d]
    [(== `(,a . ,d) quasi-p)
     (=/= 'unquote a)]
    [(later (conde
             [(now (== penv penv-out)) ;; TODO: could this get lost?
              (u-literalo mval)]
             [(fresh (penv^ v1 v2)
                (== `(,v1 . ,v2) mval)
                (conde
                 [(now (quasi-p-no-match a v1 penv penv-out))]
                 [(now (quasi-p-match a v1 penv penv^))
                  (now (quasi-p-no-match d v2 penv^ penv-out))]))]))])))

(defrel/generator (evalo-staged expr val)
  (eval-expo expr initial-env val))
