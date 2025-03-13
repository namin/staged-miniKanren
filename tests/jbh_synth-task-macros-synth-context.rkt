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

(record-bench 'synth/ground-context 'staging 'synth-append 1)
(defrel (synth-append e)
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

(record-bench 'synth/ground-context 'staged 'synth-append 1)
(time-test
 #:times 100
 (run 1 (e)
   (synth-append e))
 '((car xs)))



(defrel (synth-append-unstaged e)
  (evalo-unstaged
   `(letrec
        ([append
          (lambda (xs ys)
            (if (null? xs) ys
                (cons ,e (append (cdr xs) ys))))])
      (list
       (append '() '())
       (append '(a) '(b))
       (append '(c d) '(e f))))
   '(() (a b) (c d e f))))

(record-bench 'synth/ground-context 'unstaged 'synth-append 1)
(time-test
 #:times 100
 (run 1 (e)
   (synth-append-unstaged e))
 '((car xs)))
