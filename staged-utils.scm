;; # Utilities for quick-fixing scope
(define union
  (lambda (a b)
    (if (null? a) b
        (union (cdr a) (if (memq (car a) b) b (cons (car a) b))))))

(define diff
  (lambda (a b)
    (if (null? b) a (remq (car b) (diff a (cdr b))))))

(define fix-scope1
  (lambda (t . in-cdr)
    (cond
      ((symbol? t)
       (list t (if (reified-var? t) (list t) (list))))
      ((and (null? in-cdr) (pair? t) (eq? 'fresh (car t)))
       (let ((r (map fix-scope1 (cddr t))))
         (let ((body (map car r))
               (vs (fold-right union
			       (filter (lambda (x) (not (reified-var? x))) (cadr t))
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
       (let ((ds (diff (cadr t) (filter reified-var? s)))
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
     (for-each
       (lambda (i x) (printf "result ~a: ~a\n" (+ 1 i) x))
       (iota (length r))
       r)
     (error 'gen "staging non-deterministic"))
    (else (car r))))
(define (layer? tag r)
  (and (pair? r) (pair? (cdr r)) (eq? tag (cadr r))))
(define (code-layer? r)
  (layer? '!! r))
(define (constraint-layer? r)
  (layer? '$$ r))
(define (maybe-remove-constraints r)
  (if (constraint-layer? r)
      (car r)
      r))
(define (convert-constraints r)
  (cond
    ((constraint-layer? r)
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
     (map (lambda (x) `(symbolo ,x)) (cdr c)))
    ((eq? (car c) 'num)
     (map (lambda (x) `(numbero ,x)) (cdr c)))
    (else (error 'process-constraint (format "unexpected constraint: ~a" c)))))

(define (miniexpand x)
  (cond
    ((reified-var? x) x)
    (else `(quote ,x))))
(define (reified-expand x)
  (cond
    ((reified-var? x) x)
    ((pair? x)
     (list 'cons
           (reified-expand (car x))
           (reified-expand (cdr x))))
    (else `(quote ,x))))
;; # Helpers for turning functional procedure into relational one
(define res #f)

(define (gen-func r . inputs)
  (let ((r (unique-result r)))
      (let ((cs (convert-constraints r))
            (r (maybe-remove-constraints r)))
        (unless (code-layer? r)
          (error 'gen-func (format "no code generated: ~a" r)))
        (set! res
              (fix-scope
               `(lambda (,@inputs out)
                  (fresh () ,@cs (== ,(reified-expand (car r)) out) . ,(caddr r)))))
        res)))

(define (gen-func-rel r . inputs)
  (let ((r (unique-result r)))
      (let ((cs (convert-constraints r))
            (r (maybe-remove-constraints r)))
        (unless (code-layer? r)
          (error 'gen-func (format "no code generated: ~a" r)))
        (set! res
              (fix-scope
               `(lambda (,@inputs)
                  (fresh () ,@cs (== ,(reified-expand (car r)) (list ,@inputs)) . ,(caddr r)))))
        res)))

(define gen
  (lambda (p-name inputs rhs . contexts)
    (let ((context (if (null? contexts) (lambda (x) x) (car contexts))))
      (apply gen-func
       (run 100 (q)
         (fresh (env inputs^)
           (ext-env*o inputs inputs^ initial-env env)
           (make-list-of-symso inputs inputs^)
           (eval-expo 
                      (context
                       `(letrec ((,p-name (lambda ,inputs ,rhs)))
                          (,p-name . ,inputs)))
                      env
                      q)))
       inputs))))

(define (gen-hole query result . extra)
  (gen-func
   (run 100 (q)
     (if (null? extra) succeed ((car extra) q))
     (eval-expo 
                (query q)
                initial-env
                result))))
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
