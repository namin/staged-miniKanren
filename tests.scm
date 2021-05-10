;; using namin/faster-miniKaren branch staged
(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(load "staged-interp.scm")
(load "staged-utils.scm")

(load "test-check.scm")

(test (ex 't '(x) 'x) '(x))
(test
  (gen 't '(x) 'x)
  '(lambda (x out)
     (fresh
         (_.0)
       (== _.0 out)
       (letrec ([t (lambda (x)
                     (lambda (_.1) (fresh () (== x _.1))))])
         (fresh (_.2) (== x _.2) (callo t _.0 (cons _.2 '())))))))
(define ido (eval (gen 't '(x) 'x)))
(test (run* (q) (ido q q)) '(_.0))

(test (ex 't '(x) '((lambda (y) y) x)) '(x))
(test
  (gen 't '(x) '((lambda (y) y) x))
  '(lambda (x out)
     (fresh
         (_.0)
       (== _.0 out)
       (letrec ([t (lambda (x)
                     (lambda (_.1) (fresh (_.2) (== x _.2) (== _.2 _.1))))])
         (fresh (_.3) (== x _.3) (callo t _.0 (cons _.3 '())))))))
(gen 't '(x) '(((lambda (y) (lambda (z) z)) x) x))
(test (ex 't '(x) '(((lambda (y) (lambda (z) z)) x) x)) '(x))
(test (ex 't '(x) '(((lambda (y) (lambda (z) z)) 5) x)) '(x))

(test (ex 't '(x) '5) '(5))
(test (gen 't '(x) '5)
      '(lambda (x out)
         (fresh (_.0)
           (== _.0 out)
           (letrec ([t (lambda (x)
                         (lambda (_.1) (fresh () (== '5 _.1))))])
             (fresh (_.2) (== x _.2) (callo t _.0 (cons _.2 '())))))))
(test (ex 't '(x) '((lambda (y) y) 5)) '(5))
(test (ex 't '(x) '(((lambda (y) (lambda (z) z)) x) 5)) '(5))

(test (ex 't '(x) '(if #t x 5)) '(x))

(test (ex 't '(x) '(letrec ((f (lambda (y) y))) 1)) '(1))

(test (ex 't '(x) '(letrec ((f (lambda (y) y))) (f x))) '(x))

(test (ex 't '(x) '(letrec ((f (lambda (y) (cons y y)))) (f x))) '((x . x)))

(test ((fwd1 (eval (gen 't '(x) '(null? x)))) '()) '(#t))
(test ((fwd1 (eval (gen 't '(x) '(null? x)))) '(a b)) '(#f))

(test ((fwd1 (eval (gen 'f '(x) '(letrec ((f (lambda (y) (if (null? y) '() (cdr y))))) (f x))))) '()) '(()))
(test ((fwd1 (eval (gen 'f '(x) '(letrec ((f (lambda (y) (if (null? y) '() (cdr y))))) (f x))))) '(a b)) '((b)))

(test (ex 'f '(x) '(letrec ((f (lambda (y) (if (null? y) '() (f (cdr y)))))) (f x))) '())
(test
  (gen 'f '(x) '(letrec ((f (lambda (y) (if (null? y) '() (f (cdr y)))))) (f x)))
'(lambda (x out)
  (fresh
    (_.0)
    (== _.0 out)
    (letrec ([f (lambda (x)
                  (lambda (_.1)
                    (fresh
                      ()
                      (letrec ([f (lambda (y)
                                    (lambda (_.2)
                                      (fresh (_.3 _.4 _.5 _.6 _.8 _.7)
                                        (== (cons _.3 '()) (cons _.4 '()))
                                        (conde
                                          ((== '() _.3) (== #t _.5))
                                          ((=/= '() _.3) (== #f _.5)))
                                        (== y _.4)
                                        (conde
                                          ((=/= #f _.5) (== _.2 '()))
                                          ((== #f _.5)
                                            (== (cons (cons _.6 _.7) '())
                                                (cons _.8 '()))
                                            (== y _.8)
                                            (callo
                                              f
                                              _.2
                                              (cons _.7 '())))))))])
                        (fresh
                          (_.9)
                          (== x _.9)
                          (callo f _.1 (cons _.9 '())))))))])
      (fresh (_.10) (== x _.10) (callo f _.0 (cons _.10 '())))))))

(test (ex 't '(x) '(letrec ((f (lambda (y) (if (null? y) '() (cons 1 (f (cdr y))))))) (f x))) '())
(test
  (gen 't '(x) '(letrec ((f (lambda (y) (if (null? y) '() (cons 1 (f (cdr y))))))) (f x)))
 '(lambda (x out)
  (fresh
    (_.0)
    (== _.0 out)
    (letrec ([t (lambda (x)
                  (lambda (_.1)
                    (fresh
                      ()
                      (letrec ([f (lambda (y)
                                    (lambda (_.2)
                                      (fresh
                                        (_.3 _.4 _.5 _.6 _.7 _.8 _.10 _.12
                                             _.11 _.9)
                                        (== (cons _.3 '()) (cons _.4 '()))
                                        (conde
                                          ((== '() _.3) (== #t _.5))
                                          ((=/= '() _.3) (== #f _.5)))
                                        (== y _.4)
                                        (conde
                                          ((=/= #f _.5) (== _.2 '()))
                                          ((== #f _.5)
                                            (== (cons _.6 (cons _.7 '()))
                                                (cons _.8 (cons _.9 '())))
                                            (== (cons _.6 _.7) _.2)
                                            (== '1 _.8)
                                            (== (cons (cons _.10 _.11) '())
                                                (cons _.12 '()))
                                            (== y _.12)
                                            (callo f _.9 (cons _.11 '())))))))])
                        (fresh (_.13) (== x _.13) (callo f _.1 (cons _.13 '())))))))])
      (fresh (_.14) (== x _.14) (callo t _.0 (cons _.14 '())))))))
(test ((fwd1 (eval (gen 't '(x) '(letrec ((f (lambda (y) (if (null? y) '() (cons 1 (f (cdr y))))))) (f x))))) '(a b)) '((1 1)))

(test (ex 't '(x) ''(a b c)) '((a b c)))
(test
 (gen 't '(x) ''(a b c))
 '(lambda (x out)
  (fresh
    (_.0)
    (== _.0 out)
    (letrec ([t (lambda (x)
                  (lambda (_.1)
                    (fresh
                      ()
                      (== _.1 (cons 'a (cons 'b (cons 'c '())))))))])
      (fresh (_.2) (== x _.2) (callo t _.0 (cons _.2 '())))))))

(define appendo
  (eval
   (gen 'append '(xs ys)
        '(if (null? xs) ys
             (cons (car xs)
                   (append (cdr xs) ys))))))

(test (run* (q) (appendo '(a) '(b) q)) '((a b)))
(test (run* (q) (appendo q '(b) '(a b))) '((a)))
(test
 (run* (q) (fresh (x y) (== q (list x y)) (appendo x y '(a b c d e))))
 '((() (a b c d e)) ((a) (b c d e)) ((a b) (c d e))
  ((a b c) (d e)) ((a b c d) (e)) ((a b c d e) ())))

(test
   (gen 'append '(xs ys)
        '(if (null? xs) ys
             (cons (car xs)
                   (append (cdr xs) ys))))
'(lambda (xs ys out)
  (fresh (_.0)
    (== _.0 out)
    (letrec ([append
              (lambda (xs ys)
                (lambda (_.1)
                  (fresh (_.2 _.3 _.4 _.5 _.6 _.9 _.7 _.10 _.11 _.13 _.12 _.14 _.8)
                    (== (cons _.2 '()) (cons _.3 '()))
                    (conde
                      ((== '() _.2) (== #t _.4))
                      ((=/= '() _.2) (== #f _.4)))
                    (== xs _.3)
                    (conde
                      ((=/= #f _.4) (== ys _.1))
                      ((== #f _.4)
                       (== (cons _.5 (cons _.6 '()))
                           (cons _.7 (cons _.8 '())))
                       (== (cons _.5 _.6) _.1)
                       (== (cons (cons _.7 _.9) '())
                           (cons _.10 '()))
                       (== xs _.10)
                       (== (cons (cons _.11 _.12) '())
                           (cons _.13 '()))
                       (== xs _.13) (== ys _.14)
                       (callo  append _.8 (cons _.12 (cons _.14 '()))))))))])
      (fresh (_.15 _.16)
        (== xs _.15)
        (== ys _.16)
        (callo append _.0 (cons _.15 (cons _.16 '()))))))))

(test
 (gen 'ex-if '(x) '(if (null? x) 1 2))
 '(lambda (x out)
  (fresh
    (_.0)
    (== _.0 out)
    (letrec ([ex-if (lambda (x)
                      (lambda (_.1)
                        (fresh (_.2 _.3 _.4) (== (cons _.2 '()) (cons _.3 '()))
                          (conde
                            ((== '() _.2) (== #t _.4))
                            ((=/= '() _.2) (== #f _.4)))
                          (== x _.3)
                          (conde
                            ((=/= #f _.4) (== '1 _.1))
                            ((== #f _.4) (== '2 _.1))))))])
      (fresh (_.5) (== x _.5) (callo ex-if _.0 (cons _.5 '())))))))
(test (run* (q) (l== q 1) (l== q 2))
      '((_.0 !! ((== _.0 '1) (== _.0 '2)))))
(test (run* (q) (conde [(l== q 1)] [(l== q 2)]))
      '((_.0 !! ((== _.0 '1))) (_.0 !! ((== _.0 '2)))))
(test (run* (q) (lift `(conde [(== ,q 1)] [(== ,q 2)])))
      '((_.0 !! ((conde ((== _.0 1)) ((== _.0 2)))))))
(define fake-evalo (lambda (q n)
                     (fresh ()
                       (l== q n)
                       (l== n n))))
(test
 (run* (q)
       (fresh (c1 c2)
              (lift-scope (fake-evalo q 1) c1)
              (lift-scope (fake-evalo q 2) c2)
              (lift `(conde ,c1 ,c2))))
 '((_.0 !!
        ((conde
          ((== _.0 '1) (== '1 '1))
          ((== _.0 '2) (== '2 '2)))))))

;; beyond appendo
;; challenge 6 of ICFP'17
(define member?o
  (eval (gen 'member? '(x ls)
             '(if (null? ls) #f
                  (if (equal? (car ls) x) #t
                      (member? x (cdr ls)))))))
(test (run* (q) (member?o 'A '(A) q)) '(#t))
(test (run* (q) (member?o 'A '(B) q)) '(#f))
(test (run* (q) (fresh (a b) (== q (list a b)) (member?o a '() b))) '((_.0 #f)))
(define proof?o
  (eval (gen 'proof? '(proof)
             '(match proof
                [`(,A ,assms assumption ()) (member? A assms)]
                [`(,B ,assms modus-ponens
                      (((,A => ,B) ,assms ,r1 ,ants1)
                       (,A ,assms ,r2 ,ants2)))
                 (and (proof? (list (list A '=> B) assms r1 ants1))
                      (proof? (list A assms r2 ants2)))]
                [`((,A => ,B) ,assms conditional
                   ((,B (,A . ,assms) ,rule ,ants)))
                 (proof? (list B (cons A assms) rule ants))])
             (lambda (x)
               `(letrec ([member?
                         (lambda (x ls)
                           (if (null? ls) #f
                               (if (equal? (car ls) x) #t
                                   (member? x (cdr ls)))))])
                  ,x)))))


(test
 (run* (q) (proof?o '(A (A (A => B) (B => C)) assumption ()) #t))
 '(_.0))
(test
 (run* (q) (proof?o '((A => B) (A (A => B) (B => C)) assumption ()) q))
 '(#t))
(test
 (run* (q) (proof?o '(B (A (A => B) (B => C))
                        modus-ponens
                        (((A => B) (A (A => B) (B => C)) assumption ())
                         (A (A (A => B) (B => C)) assumption ()))) q))
 '(#t))
(test
 (run 1 (prf)
      (fresh (body)
             ;; prove C holds, given A, A => B, B => C
             (== prf `(C (A (A => B) (B => C)) . ,body))
             (proof?o prf #t)))
 '((C (A (A => B) (B => C))
    modus-ponens
    (((B => C) (A (A => B) (B => C)) assumption ())
      (B (A (A => B) (B => C))
         modus-ponens
         (((A => B) (A (A => B) (B => C)) assumption ())
           (A (A (A => B) (B => C)) assumption ())))))))
(time-test
 (run 1 (prf)
   (fresh (body)
     ;; prove (A => B) => (B => C) => (A => C) holds absolutely
     (== prf `(((A => B) => ((B => C) => (A => C))) () . ,body))
     (proof?o prf #t)))
 '((((A => B) => ((B => C) => (A => C)))
   ()
   conditional
   ((((B => C) => (A => C))
      ((A => B))
      conditional
      (((A => C)
         ((B => C) (A => B))
         conditional
         ((C (A (B => C) (A => B))
             modus-ponens
             (((B => C) (A (B => C) (A => B)) assumption ())
               (B (A (B => C) (A => B))
                  modus-ponens
                  (((A => B) (A (B => C) (A => B)) assumption ())
                    (A (A (B => C) (A => B)) assumption ())))))))))))))

(time-test
 (run 1 (prf)
   (fresh (body)
     ;; prove commutativity of ∧, encoded with =>
     ;; ((A ∧ B) => (B ∧ A))
     ;; (¬(¬A ∨ ¬B) => ¬(¬B ∨ ¬A))
     ;; (¬(A => ¬B) => ¬(B => ¬A))
     ;; (((A => (B => C)) => C) => ((B => (A => C)) => C))
     (== prf `((((A => (B => C)) => C) => ((B => (A => C)) => C)) () . ,body))
     (proof?o prf #t)))
 '(((((A => (B => C)) => C) => ((B => (A => C)) => C))
    ()
    conditional
    ((((B => (A => C)) => C)
      (((A => (B => C)) => C))
      conditional
      ((C ((B => (A => C)) ((A => (B => C)) => C))
          modus-ponens
          ((((A => (B => C)) => C)
            ((B => (A => C)) ((A => (B => C)) => C))
            assumption
            ())
           ((A => (B => C))
            ((B => (A => C)) ((A => (B => C)) => C))
              conditional
              (((B => C)
                (A (B => (A => C)) ((A => (B => C)) => C))
                conditional
                ((C (B A (B => (A => C)) ((A => (B => C)) => C))
                     modus-ponens
                     (((A => C)
                       (B A (B => (A => C)) ((A => (B => C)) => C))
                       modus-ponens
                       (((B => (A => C))
                         (B A (B => (A => C)) ((A => (B => C)) => C))
                           assumption
                           ())
                        (B (B A (B => (A => C)) ((A => (B => C)) => C))
                           assumption
                           ())))
                      (A (B A (B => (A => C)) ((A => (B => C)) => C))
                          assumption
                          ())))))))))))))))


;; running with holes
(load "unstaged-interp.scm")

(test
    (syn-hole 1
     (lambda (q)
       `(letrec ((append
                  (lambda (xs ys)
                    (if (null? xs) ,q
                        (cons (car xs) (append (cdr xs) ys))))))
          (append '(1 2) '(3 4))))
     '(1 2 3 4))
 '(ys))

;; mutually-recursive
(test
 (run 1 (q)
      (eval-expo #t
                 `(letrec ((even? (lambda (n)
                                    (if (equal? n 'z) #t
                                        (if (equal? n '(s z)) #f
                                            (odd? (car (cdr n)))))))
                           (odd? (lambda (n)
                                   (if (equal? n 'z) #f
                                       (if (equal? n '(s z)) #t
                                           (even? (car (cdr n))))))))
                    (even? '(s (s (s z)))))
                 initial-env
                 q))
  '((_.0 !!
      ((letrec ([even? (lambda (n)
                         (lambda (_.1)
                           (fresh ()
                             (== (cons _.2 (cons _.3 '()))
                                 (cons _.4 (cons _.5 '())))
                             (conde
                               ((== _.2 _.3) (== #t _.6))
                               ((=/= _.2 _.3) (== #f _.6)))
                             (== n _.4) (== _.5 'z)
                             (conde
                               ((=/= #f _.6) (== '#t _.1))
                               ((== #f _.6)
                                 (== (cons _.7 (cons _.8 '()))
                                     (cons _.9 (cons _.10 '())))
                                 (conde
                                   ((== _.7 _.8) (== #t _.11))
                                   ((=/= _.7 _.8) (== #f _.11)))
                                 (== n _.9)
                                 (== _.10 (cons 's (cons 'z '())))
                                 (conde
                                   ((=/= #f _.11) (== '#f _.1))
                                   ((== #f _.11)
                                     (== (cons (cons _.12 _.13) '())
                                         (cons _.14 '()))
                                     (== (cons (cons _.15 _.14) '())
                                         (cons _.16 '()))
                                     (== n _.16)
                                     (callo odd? _.1 (cons _.12 '())))))))))]
                [odd? (lambda (n)
                        (lambda (_.17)
                          (fresh ()
                            (== (cons _.18 (cons _.19 '()))
                                (cons _.20 (cons _.21 '())))
                            (conde
                              ((== _.18 _.19) (== #t _.22))
                              ((=/= _.18 _.19) (== #f _.22)))
                            (== n _.20) (== _.21 'z)
                            (conde
                              ((=/= #f _.22) (== '#f _.17))
                              ((== #f _.22)
                                (== (cons _.23 (cons _.24 '()))
                                    (cons _.25 (cons _.26 '())))
                                (conde
                                  ((== _.23 _.24) (== #t _.27))
                                  ((=/= _.23 _.24) (== #f _.27)))
                                (== n _.25)
                                (== _.26 (cons 's (cons 'z '())))
                                (conde
                                  ((=/= #f _.27) (== '#t _.17))
                                  ((== #f _.27)
                                    (== (cons (cons _.28 _.29) '())
                                        (cons _.30 '()))
                                    (== (cons (cons _.31 _.30) '())
                                        (cons _.32 '()))
                                    (== n _.32)
                                    (callo even? _.17 (cons _.28 '())))))))))])
         (fresh
           ()
           (== _.33
               (cons
                 's
                 (cons (cons 's (cons (cons 's (cons 'z '())) '())) '())))
           (callo even? _.0 (cons _.33 '()))))))))

(define (my-map f xs)
  (if (null? xs) '()
      (cons (f (car xs))
            (my-map f (cdr xs)))))

(my-map (lambda (x) (+ x 1)) '(1 2 3))

(define my-mapo
  (eval
   (gen 'my-map '(f xs)
        '(if (null? xs) '()
             (cons (f (car xs))
                   (my-map f (cdr xs)))))))

(define h1o
  (eval
   (gen 'h1 '(f)
        '(f 1))))

(todo "closure hole"
 (run 2
      (q)
      (h1o q 2))
 '((closure _.0 _.1 _.2)))

(todo "map car"
 (run 1
      (q)
      (my-mapo
       q
       '((1) (2) (3))
       '(1 2 3)))
 '((prim . car)))

(gen 'foo '(x)
     '(lambda (y) ((foo 1) 2)))

;; currying is painful
(define curried-appendo
  (eval
   (gen 'curried-append '(xs)
        '(lambda (ys)
           (if (null? xs) ys
               (cons (car xs)
                     ((curried-append (cdr xs)) ys)))))))

(define opt-curried-appendo
  (eval
   (gen 'opt-curried-append '(xs)
        '(if (null? xs) (lambda (ys) ys)
             (lambda (ys)
               (cons (car xs)
                     ((opt-curried-append (cdr xs)) ys)))))))

(test
  (run* (q)
    (fresh (p)
      (curried-appendo '(a) p)
      (callo p q '((b)))))
  '((a b)))

;; the hard way is no longer necessary!
(test
 (run* (q)
       (fresh (p)
              (curried-appendo '(a) p)
              (fresh (l e c)
                     (== p `(closure ,l ,e ,c))
                     (u-eval-expo (list l (list 'quote '(b))) e q))))
 '((a b)))

(test
 (run* (q)
       (fresh (p)
              (opt-curried-appendo '(a) p)
              (fresh (l e c)
                     (== p `(closure ,l ,e ,c))
                     (u-eval-expo (list l (list 'quote '(b))) e q))))
 '((a b)))

(test
 (run* (q)
       (fresh (p)
              (curried-appendo q p)
              (fresh (l e c)
                     (== p `(closure ,l ,e ,c))
                     (u-eval-expo (list l (list 'quote '(b))) e '(a b)))))
 '((a)))

(test
 (run* (q)
       (fresh (p)
              (opt-curried-appendo q p)
              (fresh (l e c)
                     (== p `(closure ,l ,e ,c))
                     (u-eval-expo (list l (list 'quote '(b))) e '(a b)))))
 '((a)))

(test
 (run* (q) (fresh (x y p)
                  (== q (list x y))
                  (curried-appendo x p)
                  (fresh (l e c)
                         (== p `(closure ,l ,e ,c))
                         (u-eval-expo (list l (list 'quote y)) e '(a b c d e)))))
 '((() (a b c d e)) ((a) (b c d e)) ((a b) (c d e))
   ((a b c) (d e)) ((a b c d) (e)) ((a b c d e) ())))

(test
 (run* (q) (fresh (x y p)
                  (== q (list x y))
                  (opt-curried-appendo x p)
                  (fresh (l e c)
                         (== p `(closure ,l ,e ,c))
                         (u-eval-expo (list l (list 'quote y)) e '(a b c d e)))))
 '((() (a b c d e)) ((a) (b c d e)) ((a b) (c d e))
   ((a b c) (d e)) ((a b c d) (e)) ((a b c d e) ())))

(test
 (run 1 (q)
      (eval-expo #t q '() 1))
 '((_.0 !! ((u-eval-expo _.0 '() '1)))))



(define ex-matcho
  (eval
   (gen 'ex-matcho '(x)
        '(match
          x
          [(? symbol? x)
           'sym-case]
          [`(,rator ,rand)
           'app-case]))))

(test
 (run 1 (q) (ex-matcho 'x q))
 '(sym-case))
(test
 (run 1 (q) (ex-matcho '(a b) q))
 '(app-case))


(define ex-matchineqo
  (eval
   (gen 'ex-matchineqo '(x)
        '(match
          x
          [`(,(? symbol? x) ,y)
           'sym-app]
          [`(1 ,y)
           'num-app]
          [`(,x ,y)
           'other-app]))))

(test
 (run* (q) (ex-matchineqo '(1 1) q))
 '(num-app))

(define ex-minimatchineqo
  (eval
   (gen 'ex-minimatchineqo '(x)
        '(match
          x
          [`(1 ,y)
           'num-app]
          [`(,x ,y)
           'other-app]))))

(test
    (run* (q) (ex-minimatchineqo '(1 1) q))
 '(num-app))

(define ex-minimatcho
  (eval
   (gen 'ex-minimatcho '(x)
        '(match
          x
          [`(1 ,y)
           'num-app]))))

(define ex-matchsym
  (eval
   (gen 'ex-matchsym '(x)
        '(match
             x
           [(? symbol? x) #t]))))

(test
    (run* (q) (ex-matchsym 'x q))
  '(#t))


(define my-lookupo
  (eval
   (gen 'lookup '(x env)
        '(match env
           [`((,y . ,v) . ,renv)
            (if (equal? x y)
                v
                (lookup x renv))]))))

(define myo
  (eval
   (gen 'lookup '(x env)
        '(match env
           [`((,x . ,y) . ,renv)
            (equal? x y)]))))

(define my-symbolo
  (eval
   (gen 'sym? '(x)
        '(symbol? x))))

(test
    (run* (q) (my-symbolo 'x q))
  '(#t))
(run* (p q) (my-symbolo p q))

(define my-not-symbolo
  (eval
   (gen 'not-sym? '(x)
        '(not (symbol? x)))))

(test
    (run* (q) (my-not-symbolo 'x q))
  '(#f))

(test
    (run 1 (q) (callo (lambda (x) (lambda (out) (== x out))) 1 `(,q)))
  '(1))

(define mb-scope
  (eval
   (gen 'mb-scope '()
        '((lambda (f) (list (not #t) (not #f))) (lambda (x) (not x))))))

(test
    (run 1 (q) (mb-scope q))
  '((#f #t)))

(test
    (syn-hole 1
              (lambda (q) `(,q 1))
              '(1 . 1)
              (lambda (q) (absento 1 q)))
  '(((lambda _.0 (cons (car _.0) (car _.0))) (=/= ((_.0 car)) ((_.0 cons))) (sym _.0))))

(test
    (length
     (run 20 (params body)
       (eval-expo #t `(lambda ,params ,body) initial-env 1)))
  1)

(test
    (length
     (run 20 (params body args)
       (eval-expo #t `((lambda ,params ,body) . ,args) initial-env 1)))
  1)

(test
    (length
     (run 1 (args)
       (eval-expo #t `(letrec ((f (lambda x x))) (f . ,args)) initial-env 1)))
  1)
