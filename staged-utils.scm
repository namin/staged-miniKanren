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
                  (ext-env*o inputs inputs^ initial-env env)
                  (make-list-of-symso inputs inputs^)
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
