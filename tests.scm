(load "mk.scm")
(load "staged-interp.scm")

(define range
  (lambda (a b)
    (if (>= a b) '() (cons a (range (+ a 1) b)))))

(define gen
  (lambda (p-name inputs rhs)
    (let ((r (car
              (run 1 (q)
                (fresh (env)
                  (ext-env*o inputs inputs initial-env env)
                  (eval-expo #t
                             `(letrec ((,p-name (lambda ,inputs ,rhs)))
                                (,p-name . ,inputs))
                             env
                             q))))))
      `(lambda ,inputs
         (run 1 (q)
           (fresh ,(map reify-name (range 0 20))
             (== q ,(car r))
             . ,(caddr r)))))))

(define ex
  (lambda (p-name inputs rhs)
    (let ((f (gen p-name inputs rhs)))
      (apply (eval f) inputs))))

(ex 't '(x) 'x)
(gen 't '(x) 'x)

(ex 't '(x) '((lambda (y) y) x))
(ex 't '(x) '(((lambda (y) (lambda (z) z)) x) x))
(ex 't '(x) '(((lambda (y) (lambda (z) z)) 5) x))

(ex 't '(x) '5)
(ex 't '(x) '((lambda (y) y) 5))
(ex 't '(x) '(((lambda (y) (lambda (z) z)) x) 5))

(ex 't '(x) '(if #t x 5))

(ex 't '(x) '(letrec ((f (lambda (y) y))) 1))

(ex 't '(x) '(letrec ((f (lambda (y) y))) (f x)))

(ex 't '(x) '(letrec ((f (lambda (y) (cons y y)))) (f x)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t
     '(letrec ((f (lambda (x) (cons x x))))
        (f x-in)) (cons `(x-in . (val . ,in)) initial-env) out)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t
    '(letrec ((f (lambda (x) (if (null? x) '() (f (cdr x))))))
        (f x-in)) (cons `(x-in . (val . ,in)) initial-env) out)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t
     '(letrec ((f (lambda (x) (if (null? x) '() (cons 1 (f (cdr x)))))))
        (f x-in)) (cons `(x-in . (val . ,in)) initial-env) out)))

(define r (car
(run 1 (q)
  (fresh (in1 in2 out)
    (== q `(,in1 ,in2 ,out))
    (eval-expo #t
     '(letrec ((append (lambda (xs ys)
                         (if (null? xs) ys
                             (cons (car xs)
                                   (append (cdr xs) ys))))))
        (append xs-in ys-in))
     (cons `(xs-in . (val . ,in1))
           (cons `(ys-in . (val . ,in2))
                 initial-env)) out)))
))

(define generated-appendo
  `(define appendo
     (lambda ,(car r)
       (fresh (...)
         ,(caddr r)))))

;; TODOs from looking at generated-appendo
;; 1. fresh (...) needs to be completed.
;; 2. pairs in unification need to be properly quoted/lifted.
;; 3. folded recursive calls could be less noisily lifted.
;; 4. it should be possible to just evaluate the generated code.
;; 5. it seems like some pair unifications could be optimized by partial lifting.
