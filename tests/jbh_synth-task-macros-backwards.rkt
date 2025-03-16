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


(record-bench 'eval/program 'staging 'invert-execute-append)
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

(record-bench 'eval/program 'staged 'invert-execute-append)
(time-test
 #:times 1000
 (run* (xs ys)
   (invert-execute-append xs ys))
 '((() (a b c))
   ((a) (b c))
   ((a b) (c))
   ((a b c) ())))

(defrel (invert-execute-append-unstaged xs ys)
  (evalo-unstaged
   `(letrec
        ([append
          (lambda (xs ys)
            (if (null? xs) ys
                (cons (car xs)
                      (append (cdr xs) ys))))])
      (append ',xs ',ys))
   '(a b c)))

(record-bench 'eval/program 'unstaged 'invert-execute-append #:description "using append split a list as in \\cref{fig:rel-interp-solutions-to-sketch-and-backwards-run} (x1000)")
(time-test
 #:times 1000
 (run* (xs ys)
   (invert-execute-append-unstaged xs ys))
 '((() (a b c))
   ((a) (b c))
   ((a b) (c))
   ((a b c) ())))
