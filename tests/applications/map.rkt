#lang racket/base

(require "../../all.rkt")


(time-test
 (run 1 (q)
   (evalo-staged
    `(letrec ([map (lambda (f l)
                     (if (null? l)
                         '()
                         (cons (f (car l))
                               (map f (cdr l)))))])
       (list
        (map (lambda (x) ,q) '(1 2 3))
        (map (lambda (x) ,q) '((1 2) (2 3)))
        (map (lambda (x) ,q) '(a b))))
    '(((1 . 1) (2 . 2) (3 . 3))
      (((1 2) . (1 2)) ((2 3) . (2 3)))
      ((a . a) (b . b)))))
   '((cons x x)))


(time-test
 (run 1 (q)
   (staged
   (evalo-staged
    `(letrec ([map (lambda (f l)
                     (if (null? l)
                         '()
                         (cons (f (car l))
                               (map f (cdr l)))))])
       (list
        (map (lambda (x) ,q) '(1 2 3))
        (map (lambda (x) ,q) '((1 2) (2 3)))
        (map (lambda (x) ,q) '(a b))))
    '(((1 . 1) (2 . 2) (3 . 3))
      (((1 2) . (1 2)) ((2 3) . (2 3)))
      ((a . a) (b . b))))))
   '((cons x x)))


 