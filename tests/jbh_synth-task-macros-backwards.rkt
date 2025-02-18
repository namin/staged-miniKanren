#lang racket/base

(require "../all.rkt")

(define-syntax-rule
  (invert-execute
   ([name (lambda (arg ...) body)])
   return)
  (run* (arg ...)
    (staged
     (evalo-staged
      `(letrec ([name (lambda (arg ...) body)])
         (name ',arg ...))
      'return))))

(test
 (invert-execute
  ([append
    (lambda (xs ys)
      (if (null? xs) ys
          (cons (car xs) (append (cdr xs) ys))))])
  (a b c))
 '((() (a b c))
   ((a) (b c))
   ((a b) (c))
   ((a b c) ())))

(test
 (run* (xs ys)
   (staged
    (evalo-staged
     `(letrec
          ([append
            (lambda (xs ys)
              (if (null? xs) ys
                  (cons (car xs)
                        (append (cdr xs) ys))))])
        (append ',xs ',ys))
     '(a b c))))
 '((() (a b c))
   ((a) (b c))
   ((a b) (c))
   ((a b c) ())))


(record-bench 'staging 'invert-execute-append 1)
(defrel (invert-execute-append xs ys)
  (time-staged
    (evalo-staged
     `(letrec
          ([append
            (lambda (xs ys)
              (if (null? xs) ys
                  (cons (car xs)
                        (append (cdr xs) ys))))])
        (append ',xs ',ys))
     '(a b c))))

(record-bench 'staged 'invert-execute-append 1)
(time-test
 (run* (xs ys)
   (invert-execute-append xs ys))
 '((() (a b c))
   ((a) (b c))
   ((a b) (c))
   ((a b c) ())))
