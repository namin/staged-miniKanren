(define ts
  (run 10 (expr val)
    (evalo-unstaged expr val)))

(define (maybe-constraints t)
  (if (constraint-layer? t)
      (cddr t)
      '()))

(define (conjunction-of gs)
  (if (null? gs)
      succeed
      (fresh ()
        (car gs)
        (conjunction-of (cdr gs)))))

(define (constraints2goal cs)
  (let ((goals (apply append (map constraint2goals cs))))
    (conjunction-of goals)))

(define (constraint2goals c)
  (cond
    ((eq? (car c) '=/=)
     (map (lambda (x) (=/= (caar x) (cadar x)))
          (cdr c)))
    ((eq? (car c) 'absento)
     (map (lambda (x) (absento (car x) (cadr x)))
          (cdr c)))
    ((eq? (car c) 'sym)
     (list (symbolo (cadr c))))
    ((eq? (car c) 'num)
     (list (numbero (cadr c))))
    (else (error 'constraint2goals "unexpected constraint" c))))

(define (shake1 t)
  (let* ((m (to-vars-map '() t))
         (t (to-vars m t))
         (cs (maybe-constraints t))
         (t (maybe-remove-constraints t)))
    (unique-result
     (run-staged 2 (expr val)
       (constraints2goal cs)
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

(test
  rs
  ts)
