#lang racket/base

(require "../../all.rkt")

(record-bench 'synth/ground-context 'unstaged 'evalo-map #:description "The body of a function mapped over several examples")
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

(record-bench 'synth/ground-context 'staging 'evalo-map)
(defrel (staged-map-query q)
  (time-staged
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


(record-bench 'synth/ground-context 'staged  'evalo-map)
(time-test
 (run 1 (q)
   (staged-map-query q))
   '((cons x x)))
