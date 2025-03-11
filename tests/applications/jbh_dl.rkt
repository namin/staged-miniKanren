#lang racket/base

(require "../../all.rkt")

;; Adapted from the nnf code in 'The Semantic Web Explained' by Péter
;; Szeredi, Gergely Lukácsy, and Tamás Benkő. Cambridge University
;; Press, 2014.
(define-term-syntax-rule (nnf the-concept)
  `(letrec ((positive?
             (lambda (n)
               (match n
                 [`z #f]
                 [`(s . ,n-1) #t]))))
     (letrec ((add1
               (lambda (n)
                 (cons 's n))))
       (letrec ((sub1
                 (lambda (n)
                   (match n
                     [`(s . ,n-1) n-1]))))
         (letrec ((nnf
                   (lambda (concept)
                     (match concept
                       [`(Not (Not ,c)) (nnf c)]
                       [`(Not (And ,c1 ,c2))
                        (list 'Or (nnf (list 'Not c1)) (nnf (list 'Not c2)))]
                       [`(Not (Or ,c1 ,c2))
                        (list 'And (nnf (list 'Not c1)) (nnf (list 'Not c2)))]
                       [`(Not (Exists ,(? symbol? r) ,c))
                        (list 'All r (nnf (list 'Not c)))]
                       [`(Not (All ,(? symbol? r) ,c))
                        (list 'Exists r (nnf (list 'Not c)))]
                       [`(Not (AtMost ,k ,(? symbol? r)))
                        (list 'AtLeast (add1 k) r)]
                       [`(Not (AtLeast ,k ,(? symbol? r)))
                        (if (positive? k)
                            (list 'AtMost (sub1 k) r)
                            (list 'Not 'Top))]
                       [`(And ,c1 ,c2)
                        (list 'And (nnf c1) (nnf c2))]
                       [`(Or ,c1 ,c2)
                        (list 'Or (nnf c1) (nnf c2))]
                       [`(Exists ,(? symbol? r) ,c)
                        (list 'Exists r (nnf c))]
                       [`(All ,(? symbol? r) ,c)
                        (list 'All r (nnf c))]
                       [concept concept]))))
           (nnf ',the-concept))))))

(record-bench 'eval-eval 'staging 'nnf)
(defrel (nnfo concept nnf-concept)
  (time-staged
   (evalo-staged
    (nnf concept)
    nnf-concept)))

(define nnfo-10-results
  `((Not Top)
  ((Not (AtLeast z _.0))
    $$
    ,not-tags0
    (sym _.0))
  (Not (Not (Not Top)))
  ((Not (Not (Not (AtLeast z _.0))))
    $$
    ,not-tags0
    (sym _.0))
  (Not (Not (Not (Not (Not Top)))))
  ((Not (Not (Not (Not (Not (AtLeast z _.0))))))
    $$
    ,not-tags0
    (sym _.0))
  (Not (Not (Not (Not (Not (Not (Not Top)))))))
  ((Not (Not (Not (Not (Not (Not (Not (AtLeast z _.0))))))))
    $$
    ,not-tags0
    (sym _.0))
  (Not (Not (Not (Not (Not (Not (Not (Not (Not Top)))))))))
  ((Not (Not (Not (Not (Not (Not (Not (Not (Not (AtLeast z _.0))))))))))
   $$
   ,not-tags0
   (sym _.0))))

(record-bench 'eval-eval 'staged 'nnf 0)
(time-test
 (length (run 10 (concept)
           (nnfo concept '(Not Top))))
 10)

(record-bench 'eval-eval 'unstaged 'nnf 0)
(time-test
 (length
  (run 10 (concept)
    (evalo-unstaged
     (nnf concept)
     '(Not Top))))
 10)

(record-bench 'eval-eval 'unstaged 'nnf 1)
(time-test
  #:times 1000
  (run* (nnf-concept)
    (evalo-unstaged
     (nnf '(Not (AtLeast z hasChild)))
     nnf-concept))
  '((Not Top)))

(record-bench 'eval-eval 'staged 'nnf 1)
(time-test
  #:times 1000
  (run* (nnf-concept)
    (nnfo '(Not (AtLeast z hasChild)) nnf-concept))
  '((Not Top)))


(record-bench 'eval-eval 'unstaged 'nnf 2)
(time-test
  #:times 1000
  (run* (nnf-concept)
    (evalo-unstaged
     (nnf '(Not (AtMost (s . z) hasChild)))
     nnf-concept))
  '((AtLeast (s s . z) hasChild)))


(record-bench 'eval-eval 'staged 'nnf 2)
(time-test
  #:times 1000
  (run* (nnf-concept)
    (nnfo '(Not (AtMost (s . z) hasChild)) nnf-concept))
  '((AtLeast (s s . z) hasChild)))

(record-bench 'eval-eval 'unstaged 'nnf 3)
(time-test
  #:times 1000
  (run* (nnf-concept)
    (evalo-unstaged
     (nnf '(Not (AtLeast (s s s . z) hasChild)))
     nnf-concept))
  '((AtMost (s s . z) hasChild)))

(record-bench 'eval-eval 'staged 'nnf 3)
(time-test
  #:times 1000
  (run* (nnf-concept)
    (nnfo '(Not (AtLeast (s s s . z) hasChild)) nnf-concept))
  '((AtMost (s s . z) hasChild)))
