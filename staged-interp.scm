(defrel/multistage/explicit (eval-expo expr env val)
  #:runtime (u-eval-expo expr env val)
  #:staging-time (s-eval-expo expr env val))

(defrel/generator (absent-tago/gen v)
  (absento 'struct v))

;; avoids an `inc` at runtime vs not-tago that has a fresh ()
(defrel (not-tago/gen v)
  (=/= 'struct v))

(defrel-partial/multistage/explicit (eval-apply-rec rep [f x* e env] [a* res])
  #:runtime
  (fresh (rep env^ env-self)
    (== rep (partial-apply eval-apply-rec f x* e env))
    (== env-self `((,f . (val . (struct rec-closure ,rep))) . ,env))
    (conde
      ((symbolo x*)
       (== env^ `((,x* . (val . ,a*)) . ,env-self)))
      ((u-ext-env*o x* a* env-self env^)))
    (u-eval-expo e env^ res))
  #:staging-time
  (fresh (env^ env-self)
    ;; TODO: should we do something like this?
    ;; (== rep (partial-apply eval-apply-rec f x* e env))
    (== env-self `((,f . (val . (struct rec-closure ,rep))) . ,env))
    ;; TODO: should be condg-ified?
    (conde
      ((symbolo x*)
       (== env^ `((,x* . (val . ,a*)) . ,env-self)))
      ((list-of-symbolso x*)
       (ext-env*o x* a* env-self env^)))
    (eval-expo e env^ res)))

(defrel-partial/multistage/explicit (eval-apply rep [x* body env] [a* val])
  #:runtime
  (fresh (env^)
    (conde
      ((symbolo x*)
       (== `((,x* . (val . ,a*)) . ,env) env^))
      ((u-ext-env*o x* a* env env^)))
    (u-eval-expo body env^ val))
  #:staging-time
  (fresh (env^)
    ;; TODO: should be condg-ified?
    (conde
      ((symbolo x*)
       (== `((,x* . (val . ,a*)) . ,env) env^))
      ((list-of-symbolso x*)
       (ext-env*o x* a* env env^)))
    (eval-expo body env^ val)))

(defrel (callo proc val a*)
  (conde
    ((fresh (rep)
       (== proc `(struct closure ,rep))
       (apply-partial rep eval-apply a* val)))
    ((fresh (rep)
       (== proc `(struct rec-closure ,rep))
       (apply-partial rep eval-apply-rec a* val)))
    ((fresh (prim-id)
       (== proc `(struct prim . ,prim-id))
       (u-eval-primo prim-id a* val)))))

(defrel/fallback (s-eval-expo expr env val) u-eval-expo
  (conde
    ;; quote
    ((fresh (v)
       (== `(quote ,v) expr)
       (absent-tago/gen v)
       (not-in-envo 'quote env)
       (later (== val v))))
    
    ;; number literal
    ((numbero expr) (later (== expr val)))

    ;; variable reference
    ((symbolo expr)
     (fresh (env-v)
       (lookupo expr env env-v)
       (later (== env-v val))))

    ;; lambda
    ((fresh (x body)
       (== `(lambda ,x ,body) expr)
       (not-in-envo 'lambda env)
       (conde
         ((symbolo x))
         ((list-of-symbolso x)))
       (later (fresh (rep)
                (== `(struct closure ,rep) val)
                (== rep (partial-apply eval-apply x body env))))))

    ;; application
    ((fresh (rator rands a* rator-v)
       (== `(,rator . ,rands) expr)

       (fallback
        ;; Behavior changes based on groundness!
        ;;
        ;; When the shape of the rator is unknown, we stage evaluation of the arguments
        ;; and generate code where this evaluation comes first. This is a different evaluation
        ;; order than we use when the proc is statically-recognizable as a primitive, and
        ;; when the unstaged interpreter encounters a primitive.
        (fresh (proc a*)
          (eval-expo rator env proc)
          (eval-listo rands env a*)
          (later (callo proc val a*)))
        ;;
        ;; Another possibility, generating duplicate code for the two branches of evaluating the
        ;; arguments. We would need something a little higher-order to avoid the duplication.
        #;(fresh (proc)
            (eval-expo rator env proc)
            (gather (conde
                     [(fresh (prim a*)
                        (later (== `(struct prim . ,prim) proc))
                        (later (u-eval-primo prim a* val))
                        (eval-listo rands env a*))]
                     [(fresh (tag p a*)
                        (later (== `(struct ,tag . ,p) proc))
                        (later (=/= tag 'prim))
                        (eval-listo rands env a*)
                        (later (callo proc val a*)))])))

        (conde
          ;; statically-recognizable primitive application
          ((fresh (prim proc a*)
             (lookupo rator env `(struct prim . ,prim))
             (eval-primo prim a* val)
             (eval-listo rands env a*)))
    
          ;; general application
          ((fresh (proc a*)
             (conde
               ((symbolo rator)
                (fresh (p tag)
                  (lookupo rator env `(struct ,tag . ,p))
                  (=/= 'prim tag)))
               ((fresh (a d) (== (cons a d) rator))))
             (eval-expo rator env proc)
             (eval-listo rands env a*)
             (later (callo proc val a*))))))))
    
    ;; match
    ((handle-matcho expr env val))
    
    ;; letrec
    ((fresh (letrec-body f x e rep)
       (== `(letrec ((,f (lambda ,x ,e)))
              ,letrec-body)
           expr)
       (not-in-envo 'letrec env)
       (later (== rep (partial-apply eval-apply-rec f x e env)))
       (eval-expo letrec-body
                  `((,f . (val . (struct rec-closure ,rep))) . ,env)
                  val)))

    ((prim-expo expr env val))))

(define empty-env '())

(defrel/fallback (lookupo x env v) u-lookupo
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
     [(== x y) (== `(val . ,v) b)]
     [(=/= x y) (lookupo x rest v)])))

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

(defrel/fallback (eval-listo expr env val) u-eval-listo
  (conde
    ((== '() expr) (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expo a env v-a)
       (eval-listo d env v-d)))))

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

(defrel/fallback (eval-primo prim-id a* val) u-eval-primo
  (conde
    [(== prim-id 'list)
     (later (== a* val))]
    [(== prim-id 'cons)
     (later (fresh (a d)
              (== `(,a ,d) a*)
              (== `(,a . ,d) val)))]
    [(== prim-id 'car)
     (fresh (d)
       (later (== `((,val . ,d)) a*))
       (later (not-tago/gen val)))]
    [(== prim-id 'cdr)
     (fresh (a)
       (later (== `((,a . ,val)) a*))
       (later (not-tago/gen a)))]
    [(== prim-id 'not)
     (later
      (fresh (b)
        (== `(,b) a*)
        (conde
          ((=/= #f b) (== #f val))
          ((== #f b) (== #t val)))))]
    [(== prim-id 'equal?)
     (later (fresh (v1 v2)
              (== `(,v1 ,v2) a*)
              (conde
                [(== v1 v2) (== #t val)]
                [(=/= v1 v2) (== #f val)])))]
    [(== prim-id 'symbol?)
     (later (fresh (v)
              (== `(,v) a*)
              (conde
                ((symbolo v) (== #t val))
                ((numbero v) (== #f val))
                ((fresh (a d)
                   (== `(,a . ,d) v)
                   (== #f val)))
                ((booleano v) (== #f val)))))]
    [(== prim-id 'number?)
     (later (fresh (v)
              (== `(,v) a*)
              (conde
                ((numbero v) (== #t val))
                ((symbolo v) (== #f val))
                ((fresh (a d)
                   (== `(,a . ,d) v)
                   (== #f val)))
                ((booleano v) (== #f val)))))]
    [(== prim-id 'pair?)
     (later (fresh (v)
              (== `(,v) a*)
              (conde
                ((symbolo v) (== #f val))
                ((numbero v) (== #f val))
                ((booleano v) (== #f val))
                ((fresh (a d)
                   (== `(,a . ,d) v)
                   (== #t val)
                   (not-tago/gen a)))
                ((fresh (a d)
                   (== `(,a . ,d) v)
                   (== #f val)
                   (pos-tago a))))))]
    [(== prim-id 'null?)
     (later (fresh (v)
              (== `(,v) a*)
              (conde
                ((== '() v) (== #t val))
                ((=/= '() v) (== #f val)))))]))

(defrel/generator (prim-expo expr env val)
  (conde
    ((boolean-primo expr env val))
    ((and-primo expr env val))
    ((or-primo expr env val))
    ((if-primo expr env val))))

(defrel/generator (boolean-primo expr env val)
  (conde
    ((== #t expr) (later (== #t val)))
    ((== #f expr) (later (== #f val)))))

(defrel/generator (and-primo expr env val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (not-in-envo 'and env)
    (ando e* env val)))

(defrel/multistage/fallback (ando e* env val)
  (conde
    ((== '() e*) (later (== #t val)))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (eval-expo e1 env v)
       (gather (conde
                 ((later (== #f v))
                  (later (== #f val)))
                 ((later (=/= #f v))
                  (ando `(,e2 . ,e-rest) env val))))))))

(defrel/generator (or-primo expr env val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (not-in-envo 'or env)
    (oro e* env val)))

(defrel/multistage/fallback (oro e* env val)
  (conde
    ((== '() e*) (later (== #f val)))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (eval-expo e1 env v)
       (gather (conde
                 ((later (=/= #f v))
                  (later (== v val)))
                 ((later (== #f v))
                  (oro `(,e2 . ,e-rest) env val))))))))

(defrel/generator (if-primo expr env val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    (eval-expo e1 env t)
    (gather (conde
              ((later (=/= #f t)) (eval-expo e2 env val))
              ((later (== #f t)) (eval-expo e3 env val))))))

(define initial-env `((list . (val . (struct prim . list)))
                      (not . (val . (struct prim . not)))
                      (equal? . (val . (struct prim . equal?)))
                      (symbol? . (val . (struct prim . symbol?)))
                      (number? . (val . (struct prim . number?)))
                      (pair? . (val . (struct prim . pair?)))
                      (cons . (val . (struct prim . cons)))
                      (null? . (val . (struct prim . null?)))
                      (car . (val . (struct prim . car)))
                      (cdr . (val . (struct prim . cdr)))
                      . ,empty-env))

(defrel/generator (handle-matcho expr env val)
  (fresh (against-expr clauses mval)
    (== `(match ,against-expr . ,clauses) expr)
    (not-in-envo 'match env)
    (eval-expo against-expr env mval)
    (match-clauses mval clauses env val)))
  
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
    ((symbolo t) (later (not-tago/gen t)))
    ((booleano/gen t))
    ((== '() t))))

(defrel/generator (booleano/gen t)
  (conde
    ((== #f t))
    ((== #t t))))

(defrel/generator (regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

(defrel/fallback (match-clauses mval clauses env val) u-match-clauses
  (conde
    ;; a match fails if no clause matches; in
    ;; unstaged this happens when reaching
    ;; match-clauses with an empty list of clauses.
    ;; in staged, defer to runtime.
    ((== clauses '()) (later fail))
    ((fresh (p result-expr d penv c-yes c-no)
       (== `((,p ,result-expr) . ,d) clauses)
       (gather (conde
                 [(fresh (env^)
                    (p-match p mval '() penv)
                    (regular-env-appendo penv env env^)
                    (eval-expo result-expr env^ val))]
                 [(p-no-match p mval '() penv)
                  (match-clauses mval d env val)]))))))

(defrel/generator (var-p-match var mval penv penv-out)
  (fresh (val)
    (later (not-tago/gen mval))
    (later (== mval val))
    (var-p-match-extend var val penv penv-out)))

(defrel/fallback (var-p-match-extend var val penv penv-out) u-var-p-match-extend
  (conde
    ((fresh (env-v)
       (match-lookupo/gen var penv env-v)
       (== penv penv-out)
       (later (== env-v val))))
    ((not-in-envo var penv)
     (== `((,var . (val . ,val)) . ,penv) penv-out))))

(defrel/fallback (var-p-no-match var mval penv penv-out) u-var-p-no-match
  (conde
    ;; a variable pattern cannot fail when it is
    ;; the first occurence of the name. unstaged
    ;; fails by failure of the lookupo below; in
    ;; staged we need to defer this failure to
    ;; runtime.
   ((not-in-envo var penv)
    (later fail))
   ((fresh (env-v)
      (== penv penv-out)
      (match-lookupo/gen var penv env-v)
      (later (=/= mval env-v))))))

(defrel/fallback (p-match p mval penv penv-out) u-p-match
  (conde
    ((self-eval-literalo p)
     (later (== p mval))
     (== penv penv-out))
    ((symbolo p) (var-p-match p mval penv penv-out))
    ((fresh (var pred)
       (== `(? ,pred ,var) p)
       (pred-match pred mval)
       (var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
       (== (list 'quasiquote quasi-p) p)
       (quasi-p-match quasi-p mval penv penv-out)))))

(defrel/fallback (pred-match pred mval) u-pred-match
  (conde
   ((== 'symbol? pred) (later (symbolo mval)))
   ((== 'number? pred) (later (numbero mval)))))

(defrel/fallback (p-no-match p mval penv penv-out) u-p-no-match
  (conde
   ((self-eval-literalo p)
    (later (=/= p mval))
    (== penv penv-out))
   ((symbolo p) (var-p-no-match p mval penv penv-out))
   ((fresh (var pred)
      (== `(? ,pred ,var) p)
      (== penv penv-out)
      (symbolo var)
      (pred-no-match pred var mval penv penv-out)))
   ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-no-match quasi-p mval penv penv-out)))))

(defrel/fallback (pred-no-match pred var mval penv penv-out) u-pred-no-match
  (conde
    ((== 'symbol? pred)
     (gather (conde
               [(later (not-symbolo mval))]
               [(later (symbolo mval))
                (var-p-no-match var mval penv penv-out)])))
    ((== 'number? pred)
     (gather (conde
               [(later (not-numbero mval))]
               [(later (numbero mval))
                (var-p-no-match var mval penv penv-out)])))))

(defrel/fallback (quasi-p-match quasi-p mval penv penv-out) u-quasi-p-match
  (conde
    ((literalo quasi-p)
     (later (== quasi-p mval))
     (== penv penv-out))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (p-match p mval penv penv-out)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (later (== `(,v1 . ,v2) mval))
       (quasi-p-match a v1 penv penv^)
       (quasi-p-match d v2 penv^ penv-out)))))

(defrel/fallback (quasi-p-no-match quasi-p mval penv penv-out) u-quasi-p-no-match
  (conde
    ((literalo quasi-p)
     (later (=/= quasi-p mval))
     (== penv penv-out))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (later (not-tago/gen mval)) ;; TODO: why do we need this?
       (p-no-match p mval penv penv-out)))
    ((fresh (a d)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (gather (conde
                 [(== penv penv-out) ;; TODO: could this get lost?
                  (later (u-literalo mval))]
                 [(fresh (penv^ v1 v2)
                    (later (== `(,v1 . ,v2) mval))
                    (conde
                      [(quasi-p-no-match a v1 penv penv-out)]
                      [(quasi-p-match a v1 penv penv^)
                       (quasi-p-no-match d v2 penv^ penv-out)]))]))))))

(defrel/generator (evalo-staged expr val)
  (eval-expo expr initial-env val))

(defrel/generator (evalo-staged/env-exts expr x* a* val)
  (fresh (env^)
    (ext-env*o x* a* initial-env env^)
    (eval-expo expr env^ val)))
