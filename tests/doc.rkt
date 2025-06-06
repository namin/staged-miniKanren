#lang racket/base

(require "../all.rkt")

(defrel (evalo e v) (evalo-unstaged e v))

;; # Background

;; ## miniKanren

(test
    (run* (q) (== q 5))
  '(5))

(test
    (run* (q) (== 5 6))
  '())

(test
    (run* (q) (== 5 5))
  '(_.0))

(test
    (run* (q) (fresh (x y) (== q `(,x ,y))))
  '((_.0 _.1)))

(test
    (run* (q) (fresh (x y) (== q `(,x ,y)) (== x 5)))
  '((5 _.0)))

(test
    (run* (q)
      (conde
        ((== q 5))
        ((== q 6))))
  '(5 6))

(test
    (run 1 (q)
      (conde
        ((== q 5))
        ((== q 6))))
  '(5))

(let ()
  (defrel (same a b)
    (== a b))
  (test
   (run 1 (p q) (same 'dog p))
   '((dog _.0)))
  (test
   (run 1 (q) (== 5 6))
   '())
  (test
   (run 1 (p q) (same (cons 'dog p) (cons p q)))
   '((dog dog)))
  (test
   (run 1 (p q) (same (cons p (cons p 'dog)) q))
   '((_.0 (_.0 _.0 . dog)))))

(let ()
  (defrel (appendo xs ys zs)
    (conde
     ((== xs '()) (== ys zs))
     ((fresh (xa xd zd)
        (== xs (cons xa xd))
        (== zs (cons xa zd))
        (appendo xd ys zd)))))

  (test
   (run* (q) (appendo '(a b) '(c d e) q))
   '((a b c d e)))

  (test
   (run* (q) (fresh (x y) (== q (list x y)) (appendo x y '(a b c d e))))
   '(
     (() (a b c d e))
     ((a) (b c d e))
     ((a b) (c d e))
     ((a b c) (d e))
     ((a b c d) (e))
     ((a b c d e) ())
     ))

  (test
   (run* (x y z) (appendo `(a  . ,x) `(,y e) `(a b c d ,z)))
   '(((b c) d e))))

(test
    (run* (q)
      (numbero q)
      (=/= 'foo q))
  '((_.0 $$ (num _.0))))

(test
    (run* (q)
      (symbolo q)
      (=/= 'foo q))
  '((_.0 $$ (=/= ((_.0 foo))) (sym _.0))))

;; ## Relational Interpreters

(test
    (run* (q) (evalo 1 q))
  '(1))

(test
    (run* (q) (evalo '(car '(1 2)) q))
  '(1))

(todo "too slow in this version of evalo"
    (run 4 (q) (evalo q q))
  '((_.0 (num _.0))
   #t
   #f
   (((lambda (_.0) (list _.0 (list 'quote _.0)))
     '(lambda (_.0) (list _.0 (list 'quote _.0))))
    (=/= ((_.0 closure))
         ((_.0 list))
         ((_.0 prim))
         ((_.0 quote)))
    (sym _.0))))

(test
    ((lambda (x) (list x (list 'quote x)))
     '(lambda (x) (list x (list 'quote x))))
  '((lambda (x) (list x (list 'quote x)))
    '(lambda (x) (list x (list 'quote x)))))

(test
    (run* (x y)
      (evalo
       `(letrec ((append (lambda (xs ys)
                           (if (null? xs) ys
                               (cons (car xs) (append (cdr xs) ys))))))
          (append ',x ',y))
       '(a b c d e)))
  '(
    (() (a b c d e))
    ((a) (b c d e))
    ((a b) (c d e))
    ((a b c) (d e))
    ((a b c d) (e))
    ((a b c d e) ())
    ))

;; # New Ground

;; ## Staging

(test
  (run* (q) (staged (later (== q 1))))
  '(1))
(generated-code)

(test
  (run* (q) (== q 1))
  '(1))

(test
  (run* (q) (staged (later (==  (list 1) (list q)))))
  '(1))
(generated-code)
;; not simplified to (== 1 _.0)

(test
  (run* (q) (staged (later (== 1 2))))
  '())
(generated-code)

(test
  (run* (q) (staged (fresh (x) (later (== (cons x x) q)))))
  '((_.0 . _.0)))
(generated-code)

;; ### run-staged

(test
    (run 1 (q)
      (staged
        (later (== q 1))))
  '(1))

#|
(run 2 (q)
  (staged
    (conde
      ((later (== q 1)))
      ((later (== q 2))))))
;; running first stage
;; Exception: staging non-deterministic
|#

(test
 (run 1 (q)
   (staged
    (fresh (p r)
      (== q (list p r))
      (== p 'dog)
      (later (== r 'fish)))))
 '((dog fish)))

(generated-code)

(defrel/staged (pet q)
  (== q 'dog))
(test
 (run 1 (q)
   (staged (pet q)))
 '(dog))
(test
 (run 1 (q)
   (staged (pet q))
   (pet q))
 '(dog))

(test
    (run 2 (q)
      (staged
        (gather (conde
                  ((later (== q 1)))
                  ((later (== q 2)))))))
    '(1 2))
(generated-code)

(test
    (run 2 (q)
      (staged
        (fallback (conde
                    ((later (== q 1)))
                    ((later (== q 2)))))))
    '(1 2))
(generated-code)

(test
 (run 2 (q)
   (staged
    (fallback (conde
               ((later (== q 1)))))))
 '(1))
(generated-code)

;; ## Staged Relational Interpreter

(test
 (run 1 (q)
      (staged
       (evalo-staged
        `(letrec ((map (lambda (f l)
                         (if (null? l)
                             '()
                             (cons (f (car l))
                                   (map f (cdr l)))))))
           (map (lambda (x) ,q) '(a b c)))
        '((a . a) (b . b) (c . c)))))
 '((cons x x)))

(test
 (run 1 (q)
      (evalo-unstaged
       `(letrec ((map (lambda (f l)
                        (if (null? l)
                            '()
                            (cons (f (car l))
                                  (map f (cdr l)))))))
          (map (lambda (x) ,q) '(a b c)))
       '((a . a) (b . b) (c . c))))
 '((cons x x)))


;; ### `appendo` as a staged relation

(defrel (appendo xs ys zs)
  (staged
   (evalo-staged
    `(letrec ((append
               (lambda (xs ys)
                 (if (null? xs)
                     ys
                     (cons (car xs)
                           (append (cdr xs) ys))))))
       (append ',xs ',ys))
    zs)))

(pretty-print (generated-code)) ;; contains the generated code

#|
(lambda (xs ys zs)
   (fresh (rep)
    (absento 'struct xs) (absento 'struct ys)
    (== rep (apply-rep 'eval-apply-rec '(append
              (xs ys)
              (if (null? xs) ys (cons (car xs) (append (cdr xs) ys)))
              <env>)
      (lambda (input out)
        (fresh (b)
         (== (list in1 in2) input)
         (disj (conj (== in1 '()) (== b #t))
               (conj (=/= in1 '()) (== b #f)))
         (disj (conj (=/= b #f) (== in2 out))
               (fresh (a r d)
                  (=/= a 'struct)
                  (== b '#f) (== out (cons a r)) (== in1 (cons a d))
                  (finish-apply rep (eval-apply-rec (list d in2) r))))))))
    (finish-apply rep (eval-apply-rec (list xs ys) zs))))
|#

;; generated code using environment extension at the top level
(defrel (appendo/env-exts xs ys zs)
  (staged
   (evalo-staged/env-exts
    '(letrec ((append
                (lambda (xs ys)
                  (if (null? xs)
                      ys
                      (cons (car xs)
                            (append (cdr xs) ys))))))
        (append xs ys))
    '(xs ys)
    `(,xs ,ys)
    zs)))

(pretty-print (generated-code))

(test
    (run* (q) (appendo '(a b) '(c d e) q))
    '((a b c d e)))

(test
    (run* (x y) (appendo x y '(a b c d e)))
  '(
    (() (a b c d e))
    ((a) (b c d e))
    ((a b) (c d e))
    ((a b c) (d e))
    ((a b c d) (e))
    ((a b c d e) ())
    ))


(defrel (context-ido e res)
  (staged
   (evalo-staged
    `(letrec ((id (lambda (x) x)))
       ,e)
    res)))

(pretty-print (generated-code))

(test
    (run* (q) (context-ido `(id 1) q))
  '(1))

(defrel (context-appendo e res)
  (staged
   (evalo-staged
    `(letrec ((append
               (lambda (xs ys)
                 (if (null? xs)
                     ys
                     (cons (car xs)
                           (append (cdr xs) ys))))))
       ,e)
    res)))

(pretty-print (generated-code))

(test
    (run* (q) (context-appendo `(append '(a b) '(c d e)) q))
    '((a b c d e)))

(test
    (length (run* (q) (context-appendo 'append q)))
  ;; ((call #<procedure append>))
  1)

(test
 (run 1 (q)
      (staged
       (evalo-staged
        `(letrec ((append
                   (lambda (xs ys)
                     (if (null? xs) ys
                         (cons (car xs) (append (cdr xs) ys))))))
           (append '(1 2) '(3 4)))
        q)))
 '((1 2 3 4)))

(test
    (run 2 (q)
         (staged
          (evalo-staged
           `(letrec ((append
                      (lambda (xs ys)
                        (if (null? xs) ys
                            (cons (car xs) (append (cdr xs) ys))))))
              (append ,q '(3 4)))
           '(1 2 3 4))))
  '('(1 2) (list 1 2)))

(test
    (run 2 (q)
      (evalo-unstaged
       `(letrec ((append
                  (lambda (xs ys)
                    (if (null? xs) ys
                        (cons (car xs) (append (cdr xs) ys))))))
          (append ,q '(3 4)))
       '(1 2 3 4)))
  '('(1 2) (list 1 2)))

;; ### Theorem checker turned prover

(defrel (proofo proof truth)
  (staged
   (evalo-staged
    `(letrec ([member?
               (lambda (x ls)
                 (if (null? ls) #f
                     (if (equal? (car ls) x) #t
                         (member? x (cdr ls)))))])
       (letrec ([proof?
                 (lambda (proof)
                   (match proof
                     [`(,A ,assms assumption ()) (member? A assms)]
                     [`(,B ,assms modus-ponens
                           (((,A => ,B) ,assms ,r1 ,ants1)
                            (,A ,assms ,r2 ,ants2)))
                      (and (proof? (list (list A '=> B) assms r1 ants1))
                           (proof? (list A assms r2 ants2)))]
                     [`((,A => ,B) ,assms conditional
                                   ((,B (,A . ,assms) ,rule ,ants)))
                      (proof? (list B (cons A assms) rule ants))]))])
         (proof? ',proof)))
    truth)))

(test
    (run 1 (prf)
      (fresh (body)
        (== prf `(C (A (A => B) (B => C)) . ,body))
        (proofo prf #t)))
  '((C (A (A => B) (B => C))
       modus-ponens
       (((B => C) (A (A => B) (B => C)) assumption ())
        (B (A (A => B) (B => C))
           modus-ponens
           (((A => B) (A (A => B) (B => C)) assumption ())
            (A (A (A => B) (B => C)) assumption ())))))))

;; ### Quines with quasiquotes

(defrel (quasi-quine-evalo expr val)
  (staged
   (evalo-staged
    `(letrec ([eval-quasi (lambda (q eval)
                            (match q
                              [(? symbol? x) x]
                              [`() '()]
                              [`(,,'`unquote ,exp) (eval exp)]
                              ;;[`(,`unquote ,exp) (eval exp)]
                              [`(quasiquote ,datum) 'error]
                              ;; ('error) in the 2017 ICFP Pearl, but
                              ;; the code generator rejects this erroneous code!
                              [`(,a . ,d)
                               (cons (eval-quasi a eval) (eval-quasi d eval))]))])
       (letrec ([eval-expr
                 (lambda (expr env)
                   (match expr
                     [`(quote ,datum) datum]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [(? symbol? x) (env x)]
                     [`(quasiquote ,datum)
                      (eval-quasi datum (lambda (exp) (eval-expr exp env)))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))])
         (eval-expr ',expr (lambda (y) 'error))))
    val)))

(todo "a tiny bit slow, left for the benchmarks"
    (run 1 (q)
      (absento 'error q)
      (quasi-quine-evalo q q))
    `((((lambda (_.0) `(,_.0 ',_.0))
      '(lambda (_.0) `(,_.0 ',_.0)))
       $$
       ,not-tags0+error
       (sym _.0))))

(test
    ((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))
  '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))


;; ### Theorem

(test
 (run 1 (q)
      (staged
       (evalo-staged
        `(letrec ((append
                   (lambda (xs ys)
                     (if (null? xs) ys
                         (cons (car xs) (append (cdr xs) ys))))))
           (append '(1 2) '(3 4)))
        q)))
 '((1 2 3 4)))

(test
    (run 1 (q)
      (evalo-unstaged
       `(letrec ((append
                  (lambda (xs ys)
                    (if (null? xs) ys
                        (cons (car xs) (append (cdr xs) ys))))))
          (append '(1 2) '(3 4)))
       q))
  '((1 2 3 4)))

;; # Implementation: Staged Relational Interpreter

(test
 (run 1 (q)
      (staged
       (fresh ()
         (symbolo q)
         (later (numbero q)))))
 '())

;; # Synthesis

(test
    (length
     (run 5 (q)
          (staged
           (evalo-staged
            q
            '(I love staged evaluation)))))
  5)

(test
    (length
     (run 5 (q)
       (evalo-unstaged
        q
        '(I love staged evaluation))))
  5)

(test
 (run 5 (q)
   (staged
    (evalo-staged
     q
     '(I love staged evaluation))))
 ;; TODO: I changed this to evalo-staged because evalo-unstaged has slightly different answer order
 ;; after the change to application in evalo-staged to get rid of explicit #:runtime
 (run 5 (q)
   (evalo-staged
    q
    '(I love staged evaluation))))

(defrel (peano-synth-fib-acc-stepo step1 step2 ACC1 ACC2)
  (staged
   (evalo-staged
    `(letrec ((zero?
               (lambda (n)
                 (equal? 'z n))))
       (letrec ((add1
                 (lambda (n)
                   (cons 's n))))
         (letrec ((sub1
                   (lambda (n)
                     (and (equal? (car n) 's)
                          (cdr n)))))
           (letrec ((+
                     (lambda (n m)
                       (if (zero? n)
                           m
                           (add1 (+ (sub1 n) m))))))
             (letrec ((-
                       (lambda (n m)
                         (if (zero? m)
                             n
                             (sub1 (- n (sub1 m)))))))
               (letrec ((fib-acc
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-acc (- n '(s . z)) ,step1 ,step2))))))
                 (list
                  (fib-acc 'z ',ACC1 ',ACC2)
                  (fib-acc '(s . z) ',ACC1 ',ACC2)
                  (fib-acc '(s s . z) ',ACC1 ',ACC2)
                  (fib-acc '(s s s . z) ',ACC1 ',ACC2)
                  (fib-acc '(s s s s . z) ',ACC1 ',ACC2)
                  (fib-acc '(s s s s s . z) ',ACC1 ',ACC2))
                 ))))))
    '(z
      (s . z)
      (s . z)
      (s s . z)
      (s s s . z)
      (s s s s s . z)))))

(time-test
 (run 1 (step1 step2 ACC1 ACC2)
   (peano-synth-fib-acc-stepo
    step1
    step2
    ACC1
    ACC2))
 '((a2 (+ a1 a2) z (s . z)))
 )

