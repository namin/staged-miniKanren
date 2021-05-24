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

(define (constraints2goal m cs)
  (let ((goals (apply append (map (lambda (c) (constraint2goals m c)) cs))))
    (conjunction-of goals)))

(define (to-sym x)
  (string->symbol (string-append "x" (symbol->string x))))

(define (constraint2goals m c)
  (cond
    ((eq? (car c) '=/=)
     (map (lambda (x) (=/= (caar x) (cadar x)))
          (cdr c)))
    ((eq? (car c) 'absento)
     (map (lambda (x) (absento (car x) (cadr x)))
          (cdr c)))
    ((eq? (car c) 'sym)
     (map (lambda (x) (== x (to-sym (cdr (assoc x m))))) (cdr c)))
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
       (constraints2goal (map (lambda (kv) (cons (cdr kv) (car kv))) m) cs)
       (== (car t) expr)
       (evalo-staged expr val)
       ;; TODO: figure out why == leads to error
       ;; in Shake 344
       ;;(l== (cadr t) val)
       ;; We can't compare values because values are not the same
       ;; e.g. for letrec Shake 125
       ))))

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
