#lang racket/base

(require "main.rkt")

(provide
 evalo-staged
 evalo-staged/env-exts
 eval-expo
 
 initial-env)

(defrel (pos-tago v)
  (== v 'struct))

(defrel (booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

(defrel/multistage (absent-tago/gen v)
  (absento 'struct v))

;; avoids an `inc` at runtime vs not-tago that has a fresh ()
(defrel (not-tago/gen v)
  (=/= 'struct v))

(defrel-partial/multistage (eval-apply-rec rep [f x* e env] [a* res])
  (fresh (env^ env-self)
    (== env-self `((,f . (val . (struct rec-closure ,rep))) . ,env))
    ;; TODO: do we need a fallback?
    (conde
      ((symbolo x*)
       (== env^ `((,x* . (val . ,a*)) . ,env-self)))
      ((ext-env*o x* a* env-self env^)))
    (eval-expo e env^ res)))

(defrel-partial/multistage (eval-apply rep [x* body env] [a* val])
  (fresh (env^)
    ;; TODO: do we need a fallback?
    (conde
      ((symbolo x*)
       (== `((,x* . (val . ,a*)) . ,env) env^))
      ((ext-env*o x* a* env env^)))
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
       (eval-primo prim-id a* val)))))


(defrel/multistage/explicit (handle-appo rator rands env val)
  #:runtime
  (fresh (a* cfun rep proc prim-id)
    (eval-expo rator env cfun)
    (conde
      ((== `(struct prim . ,prim-id) cfun)
       (eval-primo prim-id a* val)
       (eval-listo rands env a*))
      ((== `(struct closure ,rep) cfun)
       (eval-listo rands env a*)
       (callo cfun val a*))
      ((== `(struct rec-closure ,rep) cfun)
       (eval-listo rands env a*)
       (callo cfun val a*))))
  #:staging-time
  (fallback
   (later (handle-appo rator rands env val))
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
        (later (callo proc val a*)))))))

(defrel/multistage/fallback (eval-expo expr env val)
  (conde
    ;; quote
    ((fresh (v)
       (== `(quote ,v) expr)
       (later (== val v))
       (absent-tago/gen v)
       (not-in-envo 'quote env)))
    
    ;; number literal
    ((numbero expr) (later (== expr val)))

    ;; variable reference
    ((symbolo expr)
     (fresh (env-v)
       (later (== env-v val))
       (lookupo expr env env-v)))

    ;; application
    ((fresh (rator rands a* rator-v)
       (== `(,rator . ,rands) expr)
       (handle-appo rator rands env val)))
    
    ;; lambda
    ((fresh (x body rep)
       (== `(lambda ,x ,body) expr)
       (later (== `(struct closure ,rep) val))
       (conde
         ((symbolo x))
         ((list-of-symbolso x)))
       (not-in-envo 'lambda env)
       (later (== rep (partial-apply eval-apply x body env)))))
    
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

(defrel/multistage/fallback (lookupo x env v)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
     [(== x y) (== `(val . ,v) b)]
     [(=/= x y) (lookupo x rest v)])))

(defrel/multistage (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))

(defrel/multistage/fallback (eval-listo expr env val)
  (conde
    ((== '() expr) (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expo a env v-a)
       (eval-listo d env v-d)))))

;; Need to make sure lambdas are well formed.
;; Grammar constraints would be useful here!
(defrel/multistage (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(defrel/multistage (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((,x . (val . ,a)) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

(defrel/multistage/fallback (eval-primo prim-id a* val)
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

(defrel/multistage (prim-expo expr env val)
  (conde
    ((boolean-primo expr env val))
    ((and-primo expr env val))
    ((or-primo expr env val))
    ((if-primo expr env val))))

(defrel/multistage (boolean-primo expr env val)
  (conde
    ((== #t expr) (later (== #t val)))
    ((== #f expr) (later (== #f val)))))

(defrel/multistage (and-primo expr env val)
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

(defrel/multistage (or-primo expr env val)
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

(defrel/multistage (if-primo expr env val)
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

(defrel/multistage (handle-matcho expr env val)
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

(defrel/multistage (self-eval-literalo t)
  (conde
    ((numbero t))
    ((booleano/gen t))))

(defrel/multistage (literalo t)
  (conde
    ((numbero t))
    ;; in the runtime version the delay from this fresh seems useful to peano-synth-fib-acc-stepo somehow.
    ((symbolo t) (fresh () (later (not-tago/gen t))))
    ((booleano/gen t))
    ((== '() t))))

(defrel/multistage (booleano/gen t)
  (conde
    ((== #f t))
    ((== #t t))))

(defrel/multistage (regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

(defrel/multistage/fallback (match-clauses mval clauses env val)
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

(defrel/multistage (var-p-match var mval penv penv-out)
  (fresh ()
    (symbolo var)
    (later (not-tago/gen mval))
    (var-p-match-extend var mval penv penv-out)))

(defrel/multistage/fallback (var-p-match-extend var val penv penv-out)
  (conde
    ((fresh (env-v)
       (lookupo var penv env-v)
       (== penv penv-out)
       (later (== env-v val))))
    ((not-in-envo var penv)
     (== `((,var . (val . ,val)) . ,penv) penv-out))))

(defrel/multistage/fallback (var-p-no-match var mval penv penv-out)
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
      (lookupo var penv env-v)
      (later (=/= mval env-v))))))

(defrel/multistage/fallback (p-match p mval penv penv-out)
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

(defrel/multistage/fallback (pred-match pred mval)
  (conde
   ((== 'symbol? pred) (later (symbolo mval)))
   ((== 'number? pred) (later (numbero mval)))))

(defrel/multistage/fallback (p-no-match p mval penv penv-out)
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

(defrel/multistage/fallback (pred-no-match pred var mval penv penv-out)
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

(defrel/multistage/fallback (quasi-p-match quasi-p mval penv penv-out)
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

(defrel/multistage/fallback (quasi-p-no-match quasi-p mval penv penv-out)
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
                  (later (literalo mval))]
                 [(fresh (penv^ v1 v2)
                    (later (== `(,v1 . ,v2) mval))
                    (conde
                      [(quasi-p-no-match a v1 penv penv-out)]
                      [(quasi-p-match a v1 penv penv^)
                       (quasi-p-no-match d v2 penv^ penv-out)]))]))))))

(defrel/multistage (evalo-staged expr val)
  (eval-expo expr initial-env val))

(defrel/multistage (evalo-staged/env-exts expr x* a* val)
  (fresh (env^)
    (ext-env*o x* a* initial-env env^)
    (eval-expo expr env^ val)))
