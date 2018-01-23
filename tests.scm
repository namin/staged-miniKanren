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

;; WUT?
(run 1 (q)
  (fresh (xs ys res env)
    (dynamic xs ys res)
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
