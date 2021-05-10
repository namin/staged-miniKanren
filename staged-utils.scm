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
  (lambda (t . in-cdr)
    (cond
      ((symbol? t)
       (list t (if (is-reified-var? t) (list t) (list))))
      ((and (null? in-cdr) (pair? t) (eq? 'fresh (car t)))
       (let ((r (map fix-scope1 (cddr t))))
         (let ((body (map car r))
               (vs (fold-right union
			       (filter (lambda (x) (not (is-reified-var? x))) (cadr t))
			       (map cadr r))))
           (list `(fresh ,vs . ,body) (list)))))
      ((and (pair? t) (eq? 'lambda (car t)) (not (null? (cdr t))))
       (let ((r (map fix-scope1 (cddr t))))
         (let ((body (map car r))
               (vs (diff (fold-right union '() (map cadr r))
                         (if (symbol? (cadr t)) (list (cadr t)) (cadr t)))))
           (list `(lambda ,(cadr t) . ,body) vs))))
      ((pair? t)
       (let ((ra (fix-scope1 (car t)))
             (rb (fix-scope1 (cdr t) #t)))
         (list (cons (car ra) (car rb)) (union (cadr ra) (cadr rb)))))
      (else (list t (list))))))

(define fix-scope2
  (lambda (t s . in-cdr)
    (cond
      ((and (null? in-cdr) (pair? t) (eq? 'fresh (car t)))
       (let ((ds (diff (cadr t) (filter is-reified-var? s)))
             (us (union (cadr t) s)))
         `(fresh ,ds . ,(map (lambda (x) (fix-scope2 x us)) (cddr t)))))
      ((and (pair? t) (eq? 'lambda (car t)) (not (null? (cdr t))))
       (let ((us (union (if (symbol? (cadr t)) (list (cadr t)) (cadr t)) s)))
         `(lambda ,(cadr t) . ,(map (lambda (x) (fix-scope2 x us)) (cddr t)))))
      ((pair? t)
       (cons (fix-scope2 (car t) s) (fix-scope2 (cdr t) s #t)))
      (else t))))

(define fix-scope
  (lambda (t)
    (car (fix-scope2 (fix-scope1 t) '()))))

(define (unique-result r)
  (cond
    ((null? r)
     (error 'gen "staging failed"))
    ((not (null? (cdr r)))
     (printf "first result: ~a\n" (car r))
     (printf "second result: ~a\n" (cadr r))
     (if (not (null? (cddr r)))
         (printf "third result: ~a\n" (caddr r)))
     (error 'gen "staging non-deterministic"))
    (else (car r))))
(define (maybe-remove-constraints r)
  (if (eq? '$$ (cadr r))
      (car r)
      r))
(define (convert-constraints r)
  (cond
    ((eq? '$$ (cadr r))
     (printf "processing constraints: ~a\n" (cddr r))
     (process-constraints (cddr r)))
    (else '())))
(define (process-constraints cs)
  (cond
    ((null? cs) '())
    (else (append (process-constraint (car cs))
                  (process-constraints (cdr cs))))))
(define (process-constraint c)
  (printf "processing constraint: ~a\n" c)
  (cond
    ((eq? (car c) '=/=)
     (map (lambda (x) (cons '=/=
                       (list (miniexpand (caar x)) (miniexpand (cadar x)))))
          (cdr c)))
    ((eq? (car c) 'absento)
     (map (lambda (x) (cons 'absento
                       (list (miniexpand (car x)) (miniexpand (cadr x)))))
          (cdr c)))
    ((eq? (car c) 'sym)
     (list `(symbolo ,(cadr c))))
    ((eq? (car c) 'num)
     (list `(numbero ,(cadr c))))
    (else (error 'process-constraint "unexpected constraint" c))))
(define (miniexpand x)
  (cond
    ((and (symbol? x)
          (let ((chars (string->list (symbol->string x))))
            (and (char=? #\_ (car chars))
                 (char=? #\. (cadr chars)))))
     x)
    (else `(quote ,x))))
;; # Helpers for turning functional procedure into relational one
(define res '())
(define gen
  (lambda (p-name inputs rhs . contexts)
    (let ((context (if (null? contexts) (lambda (x) x) (car contexts))))
      (let ((r (run 3 (q)
                  (fresh (env inputs^)
                    (ext-env*o inputs inputs^ initial-env env)
                    (make-list-of-symso inputs inputs^)
                    (eval-expo #t
                               (context
                                `(letrec ((,p-name (lambda ,inputs ,rhs)))
                                   (,p-name . ,inputs)))
                               env
                               q)))))
        (let ((r (unique-result r)))
          (let ((cs (convert-constraints r))
                (r (maybe-remove-constraints r)))
            (set! res
                  (fix-scope
                   `(lambda (,@inputs out)
                      (fresh ()
                        ,@cs
                        (== ,(car r) out)
                            . ,(caddr r))))))
          res)))))

(define (gen-hole query result . extra)
  (let ((r (run 3 (q)
             (if (null? extra) succeed ((car extra) q))
             (eval-expo #t
                        (query q)
                        initial-env
                        result))))
    (let ((r (unique-result r)))
      (let ((cs (convert-constraints r))
            (r (maybe-remove-constraints r)))
        (fix-scope
         `(lambda (,(car r)) (fresh () ,@cs . ,(caddr r))))))))
(define (syn-hole n query result . extra)
  (printf "running first stage\n")
  (let ((e (eval (apply gen-hole query result
                        (if (null? extra) '() (cdr extra))))))
    (printf "running second stage\n")
    (run n (q)
      (if (null? extra) succeed ((car extra) q))
      (e q))))

(define ex
  (lambda (p-name inputs rhs)
    (let ((r (eval (gen p-name inputs rhs))))
      (run 1 (q)
        (apply r (append inputs (list q)))))))

(define fwd1
  (lambda (r)
    (lambda (x)
      (run* (q) (r x q)))))
