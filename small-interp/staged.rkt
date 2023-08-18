#lang racket/base

(require "../main.rkt"
         racket/pretty)

(defrel/multistage (not-tago v)
  (=/= 'struct v))

(defrel/multistage (absent-tago v)
  (absento 'struct v))

(define empty-env '())

(defrel/multistage (lookupo x env t)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (== t b))
      ((=/= x y)
       (lookupo x rest t)))))

(defrel/multistage (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((,x . ,a) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

(defrel/multistage (evalo expr val)
  (eval-expo expr empty-env val))

(defrel/multistage/fallback (eval-expo expr env val)
  (fresh ()
    ;;(trace eval-expo val)
    (conde
      ((== `(quote ,val) expr)
       (absent-tago val))

      ((symbolo expr) (lookupo expr env val))

      ((fresh (e v d)
         (== `(car ,e) expr)
         (== `(,val . ,d) v)
         (not-tago val)
         (eval-expo e env v)))

      ((fresh (e v a)
         (== `(cdr ,e) expr)
         (== `(,a . ,val) v)
         (not-tago a)
         (eval-expo e env v)))

      ((fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (eval-expo e1 env v1)
         (eval-expo e2 env v2)))

      ((fresh (e1 e2 e3 t)
         (== `(if (null? ,e1) ,e2 ,e3) expr)
         (eval-expo e1 env t)
         (gather
          (conde
            ((== t '()) (eval-expo e2 env val))
            ((=/= t '()) (eval-expo e3 env val))))))
    
      ((fresh (rator rands a* rep)
         (== `(,rator . ,rands) expr)
         (eval-expo rator env `(struct rec-closure ,rep))
         (eval-listo rands env a*)
         (later (finish-apply rep eval-apply-rec a* val))))

      ((fresh (letrec-body f x e rep)
         (== `(letrec ((,f (lambda ,x ,e)))
                ,letrec-body)
             expr)
         (== rep (specialize-partial-apply eval-apply-rec f x e env))
         (eval-expo letrec-body
                    `((,f . (struct rec-closure ,rep)) . ,env)
                    val))))))

(defrel-partial/multistage (eval-apply-rec rep [f x* e env] [a* res])
  (fresh (env^ env-self)
    (== env-self `((,f . (struct rec-closure ,rep)) . ,env))
    (ext-env*o x* a* env-self env^)
    (eval-expo e env^ res)))

(defrel/multistage (eval-listo expr env val)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expo a env v-a)
       (eval-listo d env v-d)))))

(module+ test
  (require "../test-check.rkt")

  (test
   (run 0 (x y)
     (time-staged
       (evalo
        `(letrec ((loop (lambda ()
                          (loop))))
           (loop))
        'hukarz)))
   '())
  
  (pretty-print (generated-code))

  (test
   (run* (x y)
     (time-staged
       (evalo
        `(letrec ((append (lambda (xs ys)
                            (if (null? xs)
                                ys
                                (cons (car xs) (append (cdr xs) ys))))))
           (append ',x ',y))
      '(a b c d e))))
   '((() (a b c d e))
     ((a) (b c d e))
     ((a b) (c d e))
     ((a b c) (d e))
     ((a b c d) (e))
     ((a b c d e) ())))
  
  (pretty-print (generated-code))

  (time
   (run 0 (q)
     (time-staged
      (fresh ()
        (absento 1 q)
        (evalo
         `(letrec ((append (lambda (xs ys)
                             (if (null? xs) ys
                                 (cons (car xs) ,q)))))
            (cons (append '() '())
                  (cons
                    (append '(a) '(b))
                    (cons
                      (append '(c d) '(e f))
                      (cons
                        (append '(a 1 c d) '(e f))
                        '())))))
         '(()
           (a b)
           (c d e f)
           (a 1 c d e f)))))))

  (pretty-print (generated-code))
  
  )

