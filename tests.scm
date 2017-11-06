(load "mk.scm")
(load "staged-interp.scm")

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t 'x-in `((x-in . (val . ,in))) out)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t '((lambda (x) x) x-in) `((x-in . (val . ,in))) out)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t '(((lambda (y) (lambda (x) x)) x-in) x-in) `((x-in . (val . ,in))) out)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t '5 `((x-in . (val .,in))) out)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t '((lambda (x) x) 5) `((x-in . (val . ,in))) out)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t '(((lambda (y) (lambda (x) x)) x-in) 5) `((x-in . (val . ,in))) out)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t '(if x-in 4 5) `((x-in . (val . ,in))) out)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t '(if #t x-in 5) `((x-in . (val . ,in))) out)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t
     '(letrec ((f (lambda (x) x)))
        1) `((x-in . (val . ,in))) out)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t
     '(letrec ((f (lambda (x) x)))
        (f x-in)) `((x-in . (val . ,in))) out)))

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
