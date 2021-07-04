(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(load "staged-interp.scm")
(load "unstaged-interp.scm")
(load "staged-utils.scm")
(load "staged-run.scm")

(load "test-check.scm")

(define evalo evalo-unstaged)

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

(define (appendo xs ys zs)
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
  '(((b c) d e)))

;; ## Relational Interpreters

(test
    (run* (q) (evalo 1 q))
  '(1))

(test
    (run* (q) (evalo (car '(1 2)) q))
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
    (run* (q) (l== q 1))
  '((_.0 !! ((== _.0 '1)))))

(test
    (run* (q) (== q 1))
  '(1))

(test
    (run* (q) (l== (list 1) (list q)))
  '((_.0 !! ((== (cons '1 '()) (cons _.0 '())))))
  ;; not simplified to ((_.0 !! ((== '1 _.0))))
  )

(test
    (run* (q) (l== 1 2))
  '((_.0 !! ((== '1 '2)))))

(test
    (run* (q) (fresh (x) (l== (cons x x) q)))
  '((_.0 !! ((== (cons _.1 _.1) _.0)))))

(test
    (run* (q) (later `(== ,(expand q) ,(expand 1))))
  '((_.0 !! ((== _.0 '1)))))

;; ### run-staged

(test
    (run-staged 1 (q)
      (l== q 1))
  '(1))

#|
(run-staged 2 (q)
  (conde
    ((l== q 1))
    ((l== q 2))))
;; running first stage
;; result 1: (_.0 !! ((== _.0 '1)))
;; result 2: (_.0 !! ((== _.0 '2)))
;; Exception: staging non-deterministic
|#

(test
    (run-staged 2 (q)
      (later `(conde
                ((== ,(expand q) 1))
                ((== ,(expand q) 2)))))
  '(1 2))

;; ## Staged Relational Interpreter

(test
    (run-staged 1 (q)
      (evalo-staged
       `(letrec ((map (lambda (f l)
                        (if (null? l)
                            '()
                            (cons (f (car l))
                                  (map f (cdr l)))))))
          (map (lambda (x) ,q) '(a b c)))
       '((a . a) (b . b) (c . c))))
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

(define-staged-relation (appendo xs ys zs)
  (evalo-staged
   `(letrec ((append
              (lambda (xs ys)
                (if (null? xs)
                    ys
                    (cons (car xs)
                          (append (cdr xs) ys))))))
      (append ',xs ',ys))
   zs))

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

;; ### Theorem checker turned prover

(define-staged-relation (proofo proof truth)
  (evalo-staged
   `(letrec ([member?
              (lambda (x ls)
                (if (null? ls) #f
                    (if (equal? (car ls) x) #t
                        (member? x (cdr ls)))))]
             [proof?
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
      (proof? ',proof))
   truth))

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

(define-staged-relation (quasi-quine-evalo expr val)
  (evalo-staged
   `(letrec ([eval-quasi (lambda (q eval)
                           (match q
                             [(? symbol? x) x]
                             [`() '()]
                             [`(,`unquote ,exp) (eval exp)]
                             [`(quasiquote ,datum) 'error]
                             ;; ('error) in the 2017 ICFP Pearl, but
                             ;; the code generator rejects this erroneous code!
                             [`(,a . ,d)
                              (cons (eval-quasi a eval) (eval-quasi d eval))]))]
             [eval-expr
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
      (eval-expr ',expr (lambda (y) 'error)))
   val))

(todo "a tiny bit slow, left for the benchmarks"
    (run 1 (q)
      (absento 'error q)
      (absento 'closure q)
      (quasi-quine-evalo q q))
  '((((lambda (_.0) `(,_.0 ',_.0))
      '(lambda (_.0) `(,_.0 ',_.0)))
     $$
     (=/= ((_.0 call)) ((_.0 closure)) ((_.0 dynamic))
          ((_.0 error)) ((_.0 prim)))
     (sym _.0))))

(test
    ((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))
  '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))


;; ### Theorem

(test
    (run-staged 1 (q)
      (evalo-staged
       `(letrec ((append
                  (lambda (xs ys)
                    (if (null? xs) ys
                        (cons (car xs) (append (cdr xs) ys))))))
          (append '(1 2) '(3 4)))
       q))
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

;; # Synthesis

(test
    (run-staged 5 (q)
      (evalo-staged
       q
       '(I love staged evaluation)))
  '('(I love staged evaluation)
    (((lambda _.0 '(I love staged evaluation))) $$ (=/= ((_.0 quote))) (sym _.0))
    ((car '((I love staged evaluation) . _.0)) $$ (absento (call _.0) (closure _.0) (dynamic _.0) (prim _.0)))
    (cons 'I '(love staged evaluation))
    (((lambda _.0 '(I love staged evaluation)) _.1) $$ (=/= ((_.0 quote))) (num _.1) (sym _.0))))

(test
    (run 5 (q)
      (evalo-unstaged
       q
       '(I love staged evaluation)))
  '('(I love staged evaluation)
    (((lambda _.0 '(I love staged evaluation))) $$ (=/= ((_.0 quote))) (sym _.0))
    ((car '((I love staged evaluation) . _.0)) $$ (absento (call _.0) (closure _.0) (dynamic _.0) (prim _.0)))
    (cons 'I '(love staged evaluation))
    (((lambda _.0 '(I love staged evaluation)) _.1) $$ (=/= ((_.0 quote))) (num _.1) (sym _.0))))

