#lang racket/base

(require "main.rkt")

(provide
 evalo-staged
 evalo-staged/env-exts
 eval-expo

 eval-rands-and-applyo
 
 initial-env
 eval-apply)

(defrel/staged (pos-tago v)
  (== v 'struct))

(defrel/staged (booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

(defrel/staged (absent-tago/gen v)
  (absento 'struct v))

;; avoids an `inc` at runtime vs not-tago that has a fresh ()
(defrel/staged (not-tago/gen v)
  (=/= 'struct v))


;;
;; Environment operations
;;

(define empty-env '(() . ()))

(define (build-env l)
  (cons (map car l) (map cdr l)))

(defrel/staged (lookupo x env v)
  (fresh (env-x env-xs env-v env-vs)
    (== `((,env-x . ,env-xs) . (,env-v . ,env-vs)) env)
    (conde
      [(== x env-x) (== v env-v)]
      [(=/= x env-x) (lookupo x `(,env-xs . ,env-vs) v)])))

(defrel/staged (not-in-envo x env)
  (fresh (env-xs env-vs)
    (== `(,env-xs . ,env-vs) env)
    (absento x env-xs)))

(defrel/staged (ext-envo x a env out)
  (fresh (env-xs env-vs)
    (== `(,env-xs . ,env-vs) env)
    (== `((,x . ,env-xs) . (,a . ,env-vs)) out)))

(defrel/staged (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (ext-envo x a env env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

(defrel/staged (regular-env-appendo env1 env2 env-out)
  (fresh (env-xs env-vs)
    (== `(,env-xs . ,env-vs) env1)
    (ext-env*o env-xs env-vs env2 env-out)))

(module+ private
  (provide empty-env initial-env build-env lookupo not-in-envo ext-envo ext-env*o regular-env-appendo))


(defrel-partial/staged/fallback (eval-apply-rec rep [f x* e env] [a* res])
  (fresh (env^ env-self)
    (ext-envo f `(struct rec-closure ,rep) env env-self)
    ;; TODO: do we need a fallback?
    (conde
      ((symbolo x*)
       (ext-envo x* a* env-self env^))
      ((ext-env*o x* a* env-self env^)))
    (eval-expo e env^ res)))

(defrel-partial/staged/fallback (eval-apply rep [x* body env] [a* val])
  (fresh (env^)
    ;; TODO: do we need a fallback?
    (conde
      ((symbolo x*)
       (ext-envo x* a* env env^))
      ((ext-env*o x* a* env env^)))
    (eval-expo body env^ val)))

(defrel/staged (handle-appo rator rands env val)
  (conde
    ((fresh (proc)
       (symbolo rator)
       (lookupo rator env proc)
       (eval-rands-and-applyo proc rands env val)))
    ((fresh (a d proc)
       (== (cons a d) rator)
       (eval-expo rator env proc)
       (eval-rands-and-applyo proc rands env val)))))



(defrel/staged/fallback (eval-rands-and-applyo proc rands env val)
  (fresh (a* rep)
    (conde
      ((== `(struct prim . ,rep) proc)
       (eval-primo rep a* val)
       (eval-listo rands env a*))
      ((== `(struct closure ,rep) proc)
       (eval-listo rands env a*)
       (later (finish-apply rep eval-apply a* val)))
      ((== `(struct rec-closure ,rep) proc)
       (eval-listo rands env a*)
       (later (finish-apply rep eval-apply-rec a* val))))))

(defrel/staged/fallback (eval-expo expr env val)
  (conde
    ;; quote
    ((fresh (v)
       (== `(quote ,v) expr)
       (== val v)
       (absent-tago/gen v)
       (not-in-envo 'quote env)))
    
    ;; number literal
    ((numbero expr) (== expr val))

    ;; variable reference
    ((symbolo expr)
     (fresh (env-v)
       (== env-v val)
       (fallback
        (lookupo expr env env-v))))

    ;; application
    ((fresh (rator rands a* rator-v)
       (== `(,rator . ,rands) expr)
       (handle-appo rator rands env val)))
    
    ;; lambda
    ((fresh (x body rep)
       (== `(lambda ,x ,body) expr)
       (== `(struct closure ,rep) val)
       (conde
         ((symbolo x))
         ((list-of-symbolso x)))
       (not-in-envo 'lambda env)
       (specialize-partial-apply rep eval-apply x body env)))
    
    ;; match
    ((handle-matcho expr env val))
    
    ;; letrec
    ((fresh (letrec-body f x e rep env^)
       (== `(letrec ((,f (lambda ,x ,e)))
              ,letrec-body)
           expr)
       (not-in-envo 'letrec env)
       (specialize-partial-apply rep eval-apply-rec f x e env)
       (ext-envo f `(struct rec-closure ,rep) env env^)
       (eval-expo letrec-body
                  env^
                  val)))

    ((prim-expo expr env val))))



(defrel/staged/fallback (eval-listo expr env val)
  (conde
    ((== '() expr) (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expo a env v-a)
       (eval-listo d env v-d)))))

;; Need to make sure lambdas are well formed.
;; Grammar constraints would be useful here!
(defrel/staged (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))



(defrel/staged/fallback (eval-primo prim-id a* val)
  (conde
    [(== prim-id 'list)
     (== a* val)]
    [(== prim-id 'cons)
     (fresh (a d)
       (== `(,a ,d) a*)
       (== `(,a . ,d) val))]
    [(== prim-id 'car)
     (fresh (d)
       (== `((,val . ,d)) a*)
       (not-tago/gen val))]
    [(== prim-id 'cdr)
     (fresh (a)
       (== `((,a . ,val)) a*)
       (not-tago/gen a))]
    [(== prim-id 'not)
     (fresh (b)
       (== `(,b) a*)
       (gather
        (conde
          ((=/= #f b) (== #f val))
          ((== #f b) (== #t val)))))]
    [(== prim-id 'equal?)
     (fresh (v1 v2)
       (== `(,v1 ,v2) a*)
       (gather
        (conde
          [(== v1 v2) (== #t val)]
          [(=/= v1 v2) (== #f val)])))]
    [(== prim-id 'symbol?)
     (fresh (v)
       (== `(,v) a*)
       (gather
        (conde
          ((symbolo v) (== #t val))
          ((numbero v) (== #f val))
          ((fresh (a d)
             (== `(,a . ,d) v)
             (== #f val)))
          ((booleano v) (== #f val)))))]
    [(== prim-id 'number?)
     (fresh (v)
       (== `(,v) a*)
       (gather
        (conde
          ((numbero v) (== #t val))
          ((symbolo v) (== #f val))
          ((fresh (a d)
             (== `(,a . ,d) v)
             (== #f val)))
          ((booleano v) (== #f val)))))]
    [(== prim-id 'pair?)
     (fresh (v)
       (== `(,v) a*)
       (gather
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
     (fresh (v)
       (== `(,v) a*)
       (gather
        (conde
          ((== '() v) (== #t val))
          ((=/= '() v) (== #f val)))))]))

(defrel/staged (prim-expo expr env val)
  (conde
    ((boolean-primo expr env val))
    ((and-primo expr env val))
    ((or-primo expr env val))
    ((if-primo expr env val))))

(defrel/staged (boolean-primo expr env val)
  (conde
    ((== #t expr) (== #t val))
    ((== #f expr) (== #f val))))

(defrel/staged (and-primo expr env val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (not-in-envo 'and env)
    (ando e* env val)))

(defrel/staged/fallback (ando e* env val)
  (conde
    ((== '() e*) (== #t val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (eval-expo e1 env v)
       (gather (conde
                 ((== #f v)
                  (== #f val))
                 ((=/= #f v)
                  (ando `(,e2 . ,e-rest) env val))))))))

(defrel/staged (or-primo expr env val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (not-in-envo 'or env)
    (oro e* env val)))

(defrel/staged/fallback (oro e* env val)
  (conde
    ((== '() e*) (== #f val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (eval-expo e1 env v)
       (gather (conde
                 ((=/= #f v)
                  (== v val))
                 ((== #f v)
                  (oro `(,e2 . ,e-rest) env val))))))))

(defrel/staged (if-primo expr env val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    (eval-expo e1 env t)
    (gather (conde
              ((=/= #f t) (eval-expo e2 env val))
              ((== #f t) (eval-expo e3 env val))))))

(define initial-env (build-env
                     `((list . (struct prim . list))
                       (not . (struct prim . not))
                       (equal? . (struct prim . equal?))
                       (symbol? . (struct prim . symbol?))
                       (number? . (struct prim . number?))
                       (pair? . (struct prim . pair?))
                       (cons . (struct prim . cons))
                       (null? . (struct prim . null?))
                       (car . (struct prim . car))
                       (cdr . (struct prim . cdr)))))

(defrel/staged (handle-matcho expr env val)
  (fresh (against-expr clauses mval)
    (== `(match ,against-expr . ,clauses) expr)
    (not-in-envo 'match env)
    (eval-expo against-expr env mval)
    (match-clauses mval clauses env val)))
  
(defrel/staged (not-symbolo t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(defrel/staged (not-numbero t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((symbolo t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(defrel/staged (self-eval-literalo t)
  (conde
    ((numbero t))
    ((booleano t))))

(defrel/staged (literalo t)
  (conde
    ((numbero t))
    ;; in the runtime version the delay from this fresh seems useful to peano-synth-fib-acc-stepo somehow.
    ((symbolo t) (fresh () (not-tago/gen t)))
    ((booleano t))
    ((== '() t))))




(defrel/staged/fallback (match-clauses mval clauses env val)
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

(defrel/staged (var-p-match var mval penv penv-out)
  (fresh ()
    (symbolo var)
    (not-tago/gen mval)
    (var-p-match-extend var mval penv penv-out)))

(defrel/staged/fallback (var-p-match-extend var val penv penv-out)
  (conde
    ((== penv penv-out)
     (lookupo var penv val))
    ((not-in-envo var penv)
     (ext-envo var val penv penv-out))))

(defrel/staged/fallback (var-p-no-match var mval penv penv-out)
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
       (=/= mval env-v)
       (lookupo var penv env-v)))))

(defrel/staged/fallback (p-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (== p mval)
     (== penv penv-out))
    ((symbolo p) (var-p-match p mval penv penv-out))
    ((fresh (var pred)
       (== `(? ,pred ,var) p)
       (pred-match pred mval)
       (var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
       (== (list 'quasiquote quasi-p) p)
       (quasi-p-match quasi-p mval penv penv-out)))))

(defrel/staged/fallback (pred-match pred mval)
  (conde
    ((== 'symbol? pred) (symbolo mval))
    ((== 'number? pred) (numbero mval))))

(defrel/staged/fallback (p-no-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (=/= p mval)
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

(defrel/staged/fallback (pred-no-match pred var mval penv penv-out)
  (conde
    ((== 'symbol? pred)
     (gather (conde
               [(not-symbolo mval)]
               [(symbolo mval)
                (var-p-no-match var mval penv penv-out)])))
    ((== 'number? pred)
     (gather (conde
               [(not-numbero mval)]
               [(numbero mval)
                (var-p-no-match var mval penv penv-out)])))))

(defrel/staged/fallback (quasi-p-match quasi-p mval penv penv-out)
  (conde
    ((literalo quasi-p)
     (== quasi-p mval)
     (== penv penv-out))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (p-match p mval penv penv-out)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== `(,v1 . ,v2) mval)
       (quasi-p-match a v1 penv penv^)
       (quasi-p-match d v2 penv^ penv-out)))))

(defrel/staged/fallback (quasi-p-no-match quasi-p mval penv penv-out)
  (conde
    ((literalo quasi-p)
     (=/= quasi-p mval)
     (== penv penv-out))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (not-tago/gen mval) ;; TODO: why do we need this?
       (p-no-match p mval penv penv-out)))
    ((fresh (a d)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (gather (conde
                 [(== penv penv-out)
                  (literalo mval)]
                 [(fresh (penv^ v1 v2)
                    (== `(,v1 . ,v2) mval)
                    (conde
                      [(quasi-p-no-match a v1 penv penv-out)]
                      [(quasi-p-match a v1 penv penv^)
                       (quasi-p-no-match d v2 penv^ penv-out)]))]))))))

(defrel/staged (evalo-staged expr val)
  (eval-expo expr initial-env val))

(defrel/staged (evalo-staged/env-exts expr x* a* val)
  (fresh (env^)
    (ext-env*o x* a* initial-env env^)
    (eval-expo expr env^ val)))
