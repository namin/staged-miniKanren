#lang racket

(require "../../all.rkt")

(define-term-syntax-rule (append-sketch-and-calls hole)
  `(letrec ([append
             (lambda (xs ys)
               (if (null? xs) ys
                   (cons ,hole (append (cdr xs) ys))))])
     (list
      (append '() '())
      (append '(a) '(b))
      (append '(c d) '(e f)))))

(time
 (run 1 (e)
   (evalo-unstaged
    (append-sketch-and-calls e)
    '(() (a b) (c d e f)))))

(time
 (run 1 (e)
   (time-staged
    (evalo-staged
     (append-sketch-and-calls e)
     '(() (a b) (c d e f))))))
(generated-code)