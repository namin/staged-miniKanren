(load "mk.scm")
(load "staged-interp.scm")

;; # Utilities for quick-fixing scope
(define union
  (lambda (a b)
    (if (null? a) b
        (union (cdr a) (if (memq (car a) b) b (cons (car a) b))))))

(define diff
  (lambda (a b)
    (if (null? b) a (remq (car b) (diff a (cdr b))))))

(define is-reified-var?
  (lambda (x)
    (let ((s (symbol->string x)))
      (and (> (string-length s) 2)
           (char=? (string-ref s 0) #\_)
           (char=? (string-ref s 1) #\.)))))

(define fix-scope1
  (lambda (t)
    (cond
      ((symbol? t)
       (list t (if (is-reified-var? t) (list t) (list))))
      ((and (pair? t) (eq? 'fresh (car t)))
       (let ((r (map fix-scope1 (cddr t))))
         (let ((body (map car r))
               (vs (fold-right union '() (map cadr r))))
           (list `(fresh ,vs . ,body) (list)))))
      ((and (pair? t) (eq? 'lambda (car t)))
       (let ((r (map fix-scope1 (cddr t))))
         (let ((body (map car r))
               (vs (diff (fold-right union '() (map cadr r))
                         (cadr t))))
           (list `(lambda ,(cadr t) . ,body) vs))))
      ((pair? t)
       (let ((ra (fix-scope1 (car t)))
             (rb (fix-scope1 (cdr t))))
         (list (cons (car ra) (car rb)) (union (cadr ra) (cadr rb)))))
      (else (list t (list))))))

(define fix-scope2
  (lambda (t s)
    (cond
      ((and (pair? t) (eq? 'fresh (car t)))
       (let ((ds (diff (cadr t) s))
             (us (union (cadr t) s)))
         `(fresh ,ds . ,(map (lambda (x) (fix-scope2 x us)) (cddr t)))))
      ((and (pair? t) (eq? 'lambda (car t)))
       (let ((us (union (cadr t) s)))
         `(lambda ,(cadr t) . ,(map (lambda (x) (fix-scope2 x us)) (cddr t)))))
      ((pair? t)
       (cons (fix-scope2 (car t) s) (fix-scope2 (cdr t) s)))
      (else t))))

(define fix-scope
  (lambda (t)
    (car (fix-scope2 (fix-scope1 t) '()))))

;; # Helpers for turning functional procedure into relational one
(define gen
  (lambda (p-name inputs rhs)
    (let ((r (car
              (run 1 (q)
                (fresh (env inputs^)
                  (make-list-of-symso inputs inputs^)
                  (ext-env*o inputs inputs^ initial-env env)
                  (eval-expo #t
                             `(letrec ((,p-name (lambda ,inputs ,rhs)))
                                (,p-name . ,inputs))
                             env
                             q))))))
      (fix-scope
       `(lambda (,@inputs out)
          (fresh ()
            (== ,(car r) out)
            . ,(caddr r)))))))

(define ex
  (lambda (p-name inputs rhs)
    (let ((r (eval (gen p-name inputs rhs))))
      (run 1 (q)
        (apply r (append inputs (list q)))))))

(define fwd1
  (lambda (r)
    (lambda (x)
      (run* (q) (r x q)))))

;; # Examples

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
