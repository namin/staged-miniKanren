(define ts
  (run 200 (expr val)
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
     (map (lambda (x) (symbolo x)) (cdr c)))
    ((eq? (car c) 'num)
     (map (lambda (x) (numbero x)) (cdr c)))
    (else (error 'constraint2goals "unexpected constraint" c))))

(define (shake1 t)
  (let* ((m (to-vars-map '() t))
         (t (to-vars m t))
         (cs (maybe-constraints t))
         (t (maybe-remove-constraints t)))
    (unique-result
     (run-staged 2 (expr val)
       (constraints2goal cs)
       (== (car t) expr)
       (evalo-staged expr val)
       (== (cadr t) val)))))

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

(define (nth xs n)
  (car (last-pair (list-head xs (+ 1 n)))))
#|
(for-each
  (lambda (t i)
    (printf "Shake ~a\n" i)
    (test
      (maybe-remove-constraints (shake1 t))
      (maybe-remove-constraints t)))
  ts (iota (length ts)))
|#

(define rs
  (map
   (lambda (t i) (printf "Shake ~a\n" i)
      (shake1 t))
   ts (iota (length ts))))

#|
(test
  rs
  ts)
|#
