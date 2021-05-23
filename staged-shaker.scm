(define ts
  (run 10 (expr val)
    (evalo-unstaged expr val)))

(run-staged* (expr val)
  (fresh (x)
    (absento 'call x)
    (absento 'closure x)
    (absento 'dynamic x)
    (absento 'prim x)
    (== expr `(quote ,x))
    (evalo-staged expr val)))

(define (shake1 t)
  (let* ((m (to-vars-map '() t))
         (t (maybe-remove-constraints t))
         (t (to-vars m t)))
    (car
     (run-staged 1 (expr val)
       (== t (list expr val))
       (evalo-staged expr val)))))

(define (to-vars-map m x)
  (cond
    ((is-reified-var? x)
     (let ((e (assoc x m)))
       (if e
           m
           (cons (cons x (var (new-scope))) m))))
    ((pair? x)
     (let ((m (to-vars-map m (car x))))
       (to-vars-map m (cdr x))))
    (else m)))

(define (to-vars m x)
  (cond
    ((is-reified-var? x) (cdr (assoc x m)))
    ((pair? x) (cons (to-vars m (car x)) (to-vars m (cdr x))))
    (else x)))

(define rs (map shake1 ts))
