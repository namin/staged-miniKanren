#lang racket/base

(require "../all.rkt")

(define-syntax-rule
  (synth/sketch
   (q ...)
   ([name rhs] ...)
   [example-call -> example-return] ...)
  (car
   (run 1 (q ...)
     (evalo-staged
      `(letrec
           ([name rhs] ...)
         (list example-call ...))
      '(example-return ...)))))

(test
 (synth/sketch (e)
               ([append
                 (lambda (xs ys)
                   (if (null? xs) ys
                       (cons ,e (append (cdr xs) ys))))])
               [(append '() '()) -> ()]
               [(append '(a) '(b)) -> (a b)]
               [(append '(c d) '(e f)) -> (c d e f)])
 '(car xs))


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

