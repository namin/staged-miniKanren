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
        ((unfold f) x-in)) `((x-in . (val . ,in))) out)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t
     '(letrec ((f (lambda (x) (cons x x))))
        ((unfold f) x-in)) (cons `(x-in . (val . ,in)) initial-env) out)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t
    '(letrec ((f (lambda (x) (if (null? x) '() ((fold f) (cdr x))))))
        (f x-in)) (cons `(x-in . (val . ,in)) initial-env) out)))

(run 1 (q)
  (fresh (in out)
    (== q `(,in ,out))
    (eval-expo #t
     '(letrec ((f (lambda (x) (if (null? x) '() (cons 1 ((fold f) (cdr x)))))))
        (f x-in)) (cons `(x-in . (val . ,in)) initial-env) out)))

(define r
(run 1 (q)
  (fresh (in1 in2 out)
    (== q `(,in1 ,in2 ,out))
    (eval-expo #t
     '(letrec ((append (lambda (xs ys)
                         (if (null? xs) ys
                             (cons (car xs)
                                   ((fold append) (cdr xs) ys))))))
        (append xs-in ys-in))
     (cons `(xs-in . (val . ,in1))
           (cons `(ys-in . (val . ,in2))
                 initial-env)) out)))
)

(define r1 (list (car (car r)) (reverse (caddr (car r)))))

(define p1 (walk* r1 (reify-S r1 '())))

(define generated-appendo
  `(define appendo
     (lambda ,(car p1)
       (fresh (...)
         ,(cadr p1)))))

;; TODOs from looking at generated-appendo
;; 1. fresh (...) needs to be completed.
;; 2. pairs in unification need to be properly quoted/lifted.
;; 3. folded recursive calls could be less noisily lifted.
;; 4. it should be possible to just evaluate the generated code.
