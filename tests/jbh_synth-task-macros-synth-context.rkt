#lang racket/base

(require "../all.rkt")

(define-syntax-rule
  (synth/sketch
   (q ...)
   ([name rhs] ...)
   [example-call -> example-return] ...)
  (car
   (run 1 (q ...)
     (staged
      (evalo-staged
       `(letrec
            ([name rhs] ...)
          (list example-call ...))
       '(example-return ...))))))
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


(test
 (run 1 (e)
   (staged
    (evalo-staged
     `(letrec
          ([append
            (lambda (xs ys)
              (if (null? xs) ys
                  (cons ,e (append (cdr xs) ys))))])
        (list
         (append '() '())
         (append '(a) '(b))
         (append '(c d) '(e f))))
     '(() (a b) (c d e f)))))
 '((car xs)))

(record-bench 'staging 'synth/context 1)
(defrel (synth/context e)
  (time-staged
    (evalo-staged
     `(letrec
          ([append
            (lambda (xs ys)
              (if (null? xs) ys
                  (cons ,e (append (cdr xs) ys))))])
        (list
         (append '() '())
         (append '(a) '(b))
         (append '(c d) '(e f))))
     '(() (a b) (c d e f)))))

(record-bench 'staged 'synth/context 1)
(time-test
 (run 1 (e)
   (synth/context e))
 '((car xs)))
