(load "staged-mk.scm")
(load "staged-interp.scm")
(load "staged-utils.scm")

(ex 't '(x) 'x)
(gen 't '(x) 'x)
(define ido (eval (gen 't '(x) 'x)))
(run* (q) (ido q q))

(ex 't '(x) '((lambda (y) y) x))
(gen 't '(x) '((lambda (y) y) x))
(ex 't '(x) '(((lambda (y) (lambda (z) z)) x) x))
(ex 't '(x) '(((lambda (y) (lambda (z) z)) 5) x))

(ex 't '(x) '5)
(gen 't '(x) '5)
(ex 't '(x) '((lambda (y) y) 5))
(ex 't '(x) '(((lambda (y) (lambda (z) z)) x) 5))

(ex 't '(x) '(if #t x 5))

(ex 't '(x) '(letrec ((f (lambda (y) y))) 1))

(ex 't '(x) '(letrec ((f (lambda (y) y))) (f x)))

(ex 't '(x) '(letrec ((f (lambda (y) (cons y y)))) (f x)))

((fwd1 (eval (gen 't '(x) '(null? x)))) '())
((fwd1 (eval (gen 't '(x) '(null? x)))) '(a b))

((fwd1 (eval (gen 'f '(x) '(letrec ((f (lambda (y) (if (null? y) '() (cdr y))))) (f x))))) '())
((fwd1 (eval (gen 'f '(x) '(letrec ((f (lambda (y) (if (null? y) '() (cdr y))))) (f x))))) '(a b))

(ex 'f '(x) '(letrec ((f (lambda (y) (if (null? y) '() (f (cdr y)))))) (f x)))
(gen 'f '(x) '(letrec ((f (lambda (y) (if (null? y) '() (f (cdr y)))))) (f x)))

(ex 't '(x) '(letrec ((f (lambda (y) (if (null? y) '() (cons 1 (f (cdr y))))))) (f x)))
(gen 't '(x) '(letrec ((f (lambda (y) (if (null? y) '() (cons 1 (f (cdr y))))))) (f x)))
((fwd1 (eval (gen 't '(x) '(letrec ((f (lambda (y) (if (null? y) '() (cons 1 (f (cdr y))))))) (f x))))) '(a b))

(ex 't '(x) ''(a b c))
(gen 't '(x) ''(a b c))

(define appendo
  (eval
   (gen 'append '(xs ys)
        '(if (null? xs) ys
             (cons (car xs)
                   (append (cdr xs) ys))))))

(run* (q) (appendo '(a) '(b) q))
(run* (q) (appendo q '(b) '(a b)))
(run* (q) (fresh (x y) (== q (list x y)) (appendo x y '(a b c d e))))

(gen 'ex-if '(x) '(if (null? x) 1 2))
(run* (q) (l== q 1) (l== q 2))
(run* (q) (conde [(l== q 1)] [(l== q 2)]))
(run* (q) (lift `(conde [(== ,q 1)] [(== ,q 2)])))
(define fake-evalo (lambda (q n)
                     (fresh ()
                       (l== q n)
                       (l== n n))))
(run* (q)
  (fresh (c1 c2)
  (lift-scope (fake-evalo q 1) c1)
  (lift-scope (fake-evalo q 2) c2)
  (lift `(conde ,c1 ,c2))))

;; blurring distinction
;; between logic variable and code

;; fully now
(run* (q) (== 1 q))

;; explicitly deferred
(run* (q) (l== 1 q))

;; implicitly deferred
(run* (q) (dynamic q) (== 1 q))
(run* (q) (dynamic q) (== (list 1) (list q)))

;; what to do about constraints?
(run* (q) (dynamic q) (=/= 1 q))

;; staging with vanilla interp!
(load "full-interp.scm")
(run* (q) (dynamic q) (eval-expo 1 '() q))

(run 1 (q)
  (fresh (arg res) (== q (list arg res))
    (eval-expo 'x `((x . (val . ,arg))) res)))

(run 1 (q)
  (fresh (arg res)
    (dynamic arg res)
    (== q (list arg res))
    (eval-expo 'x `((x . (val . ,arg))) res)))

;; The examples with if show that each branch is grounded
;; into an answer.

(run 1 (q)
  (fresh (arg res env)
    (dynamic arg res)
    (== q (list arg res))
    (ext-env*o '(x) `(,arg) initial-env env)
    (eval-expo '(if (null? '()) 1 2) env res)))

(run* (q)
  (fresh (arg res env)
    (== q (list arg res))
    (ext-env*o '(x) `(,arg) initial-env env)
    (eval-expo '(if (null? x) 1 2) env res)))

(run* (q)
  (fresh (arg res env)
    (dynamic arg res)
    (== q (list arg res))
    (ext-env*o '(x) `(,arg) initial-env env)
    (eval-expo '(if (null? x) 1 2) env res)))
;; =>
'(((_.0 _.1) !! ((== '() _.0) (== _.1 '1)))
  (((_.0 _.1) !! ((== _.2 _.0) (== _.1 '2)))
   (=/= ((_.2 ())))))

;; The if-grounding means that for a recursive relation,
;; we ground on the recursive structure...
;; This is not symbolic enough, it's suggestive
;; of limitations SMT has with recursion.
(run 2 (q)
  (fresh (xs ys res env)
    (dynamic xs ys)
    (== q (list xs ys res))
    (ext-env*o '(xs ys) (list xs ys) initial-env env)
    (eval-expo `(letrec
                    ((append (lambda (xs ys)
                               (if (null? xs) ys
                                   (cons (car xs)
                                         (append (cdr xs) ys))))))
                  (append xs ys))
               env
               res)))

;; next step
;; what is an alternative to semantics to if-grounding for dynamic variables?

;; here is one alternative:
(load "dynamic-interp.scm")
(define appendo
  (eval
   (gen 'append '(xs ys)
        '(if (null? xs) ys
             (cons (car xs)
                   (append (cdr xs) ys))))))

(run* (q) (appendo '(a) '(b) q))
(run* (q) (appendo q '(b) '(a b)))
(run* (q) (fresh (x y) (== q (list x y)) (appendo x y '(a b c d e))))

;; beyond appendo
;; challenge 6 of ICFP'17
(define member?o
  (eval (gen 'member? '(x ls)
             '(if (null? ls) #f
                  (if (equal? (car ls) x) #t
                      (member? x (cdr ls)))))))
(run* (q) (member?o 'A '(A) q))
(run* (q) (member?o 'A '(B) q))
(run* (q) (fresh (a b) (== q (list a b)) (member?o a '() b)))
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


(run 10 (q) (proof?o q #t))
(run* (q) (proof?o '(A (A (A => B) (B => C)) assumption ()) #t))
(run* (q) (proof?o '((A => B) (A (A => B) (B => C)) assumption ()) q))
(run* (q) (proof?o '(B (A (A => B) (B => C))
        modus-ponens
        (((A => B) (A (A => B) (B => C)) assumption ())
          (A (A (A => B) (B => C)) assumption ()))) q))

(run 1 (prf)
  (fresh (body)
    ;; prove C holds, given A, A => B, B => C
    (== prf `(C (A (A => B) (B => C)) . ,body))
    (proof?o prf #t)))

(time
 (run 1 (prf)
   (fresh (body)
     ;; prove (A => B) => (B => C) => (A => C) holds absolutely
     (== prf `(((A => B) => ((B => C) => (A => C))) () . ,body))
     (proof?o prf #t))))

(time
 (run 1 (prf)
   (fresh (body)
     ;; prove commutativity of ∧, encoded with =>
     ;; ((A ∧ B) => (B ∧ A))
     ;; (¬(¬A ∨ ¬B) => ¬(¬B ∨ ¬A))
     ;; (¬(A => ¬B) => ¬(B => ¬A))
     ;; (((A => (B => C)) => C) => ((B => (A => C)) => C))
     (== prf `((((A => (B => C)) => C) => ((B => (A => C)) => C)) () . ,body))
     (proof?o prf #t))))


;; running with holes
(load "unstaged-interp.scm")
(define (gen-hole query result)
  (let ((r (run 1 (q)
             (eval-expo #t
                        (query q)
                        initial-env
                        (quasi result)))))
    (let ((r (car r)))
      (fix-scope
       `(lambda (,(car r)) (fresh () . ,(caddr r)))))))
(define (syn-hole query result)
  (let ((e (eval (gen-hole query result))))
    (run 1 (q) (e q))))

(syn-hole
  (lambda (q)
   `(letrec ((append
              (lambda (xs ys)
                (if (null? xs) ,q
                    (cons (car xs) (append (cdr xs) ys))))))
      (append '(1 2) '(3 4))))
  '(1 2 3 4))

;; mutually-recursive
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

;; this requires
;; https://github.com/namin/faster-miniKanren/tree/staged
(run 1 (q)
  (eval-expo
   #t
   `(letrec
       ((assp
         (lambda (p l)
           (if (null? l) #f
               (if (p (car (car l))) (car l)
                   (assp p (cdr l))))))

        (var
         (lambda (c) (cons 'var c)))
        (var?
         (lambda (x) (and (pair? x) (equal? (car x) 'var))))
        (var=?
         (lambda (x1 x2) (equal? (cdr x1) (cdr x2))))

        (walk
         (lambda (u s)
           ((lambda (pr) (if pr (walk (cdr pr) s) u))
            (and (var? u) (assp (lambda (v) (var=? u v)) s)))))

        (ext-s
         (lambda (x v s)
           (cons (cons x v) s)))

        (==
         (lambda (u v)
           (lambda (s/c)
             ((lambda (s) (if s (unit (cons s (cdr s/c))) (mzero)))
              (unify u v (car s/c))))))

                (unit
         (lambda (s/c)
           (cons s/c (mzero))))
        (mzero
         (lambda ()
           '()))

        (unify
         (lambda (u v s)
           ((lambda (u v)
              (if (and (var? u) (var? v) (var=? u v)) s
                  (if (var? u) (ext-s u v s)
                      (if (var? v) (ext-s v u s)
                          (if (and (pair? u) (pair? v))
                              ((lambda (s) (and s (unify (cdr u) (cdr v) s)))
                               (unify (car u) (car v) s))
                              (and (equal? u v) s))))))
            (walk u s) (walk v s))))

        (call/fresh
         (lambda (f)
           (lambda (s/c)
             ((lambda (c) ((f (var c)) (cons (car s/c) (cons 's c))))
              (cdr s/c)))))

        (disj
         (lambda (g1 g2)
           (lambda (s/c) (mplus (g1 s/c) (g2 s/c)))))
        (conj
         (lambda (g1 g2)
           (lambda (s/c) (bind (g1 s/c) g2))))

        (mplus
         (lambda ($1 $2)
           (if (null? $1) $2
               (if (pair? $1) (cons (car $1) (mplus (cdr $1) $2))
                   (lambda () (mplus $2 ($1)))))))

        (bind
         (lambda ($ g)
           (if (null? $) (mzero)
               (if (pair? $) (mplus (g (car $)) (bind (cdr $) g))
                   (lambda () (bind ($) g))))))

        (empty-state
         (lambda ()
           '(() . z)))

        (pull
         (lambda ($)
           (if (or (null? $) (pair? $)) $ (pull ($)))))

        (take-all
         (lambda ($)
           ((lambda ($) (if (null? $) '() (cons (car $) (take-all (cdr $)))))
            (pull $))))

        (take
         (lambda (n $)
           (if (equal? n 'z) '()
               ((lambda ($) (if (null? $) '() (cons (car $) (take (cdr n) (cdr $)))))
                (pull $)))))
)

     1

     )

   initial-env
   q))
