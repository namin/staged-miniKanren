(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(load "staged-interp.scm")
(load "staged-utils.scm")
(load "staged-run.scm")

(load "unstaged-interp.scm")

(load "test-check.scm")

;; Adapted from https://github.com/k-tsushima/Shin-Barliman/blob/master/transformations/peano.scm
;; and https://github.com/k-tsushima/Shin-Barliman/blob/master/transformations/peano-relational.scm

(define (peano-synth-fib-direct fib-skeleton)
  `(letrec ((zero?
             (lambda (n)
               (equal? 'z n))))

   (letrec ((add1
             (lambda (n)
               (cons 's n))))
   (letrec ((sub1
             (lambda (n)
               (and (equal? (car n) 's)
                    (cdr n)))))
   (letrec ((+
             (lambda (n m)
               (if (zero? n)
                   m
                   (add1 (+ (sub1 n) m))))))
   (letrec ((-
             (lambda (n m)
               (if (zero? m)
                   n
                   (sub1 (- n (sub1 m)))))))

   (letrec ((fib ,fib-skeleton))
     (list
       (fib 'z)
       (fib '(s . z))
       (fib '(s s . z))
       (fib '(s s s . z))
       (fib '(s s s s . z))
       (fib '(s s s s s . z)))
     )))))))





(record-bench 'run-staged 'peano-synth-fib-direct-1)
(time-test
  (run-staged #f (fib-direct)
    (== `(lambda (n)
           (if (zero? n)
               'z
               (if (zero? (sub1 n))
                   '(s . z)
                   (+ (fib (sub1 n)) (fib (sub1 (sub1 n)))))))
        fib-direct)
    (evalo-staged
     (peano-synth-fib-direct fib-direct)
     '(z
       (s . z)
       (s . z)
       (s s . z)
       (s s s . z)                   
       (s s s s s . z))))
  '((lambda (n)
       (if (zero? n)
           'z
           (if (zero? (sub1 n))
               '(s . z)
               (+ (fib (sub1 n)) (fib (sub1 (sub1 n)))))))))

(record-bench 'unstaged 'peano-synth-fib-direct-1)
(time-test
  (run #f (fib-direct)
    (== `(lambda (n)
           (if (zero? n)
               'z
               (if (zero? (sub1 n))
                   '(s . z)
                   (+ (fib (sub1 n)) (fib (sub1 (sub1 n)))))))
        fib-direct)
    (evalo-unstaged
     (peano-synth-fib-direct fib-direct)
     '(z
       (s . z)
       (s . z)
       (s s . z)
       (s s s . z)                   
       (s s s s s . z))))
  '((lambda (n)
       (if (zero? n)
           'z
           (if (zero? (sub1 n))
               '(s . z)
               (+ (fib (sub1 n)) (fib (sub1 (sub1 n)))))))))


;; Attempt to synthesize part of the definition of fib-aps.
;; Can try to synthesize the initial accumulator arguments as well.

(define (peano-synth-fib-aps fib-aps-skeleton ACC1 ACC2)
  `(letrec ((zero?
             (lambda (n)
               (equal? 'z n))))

   (letrec ((add1
             (lambda (n)
               (cons 's n))))
   (letrec ((sub1
             (lambda (n)
               (and (equal? (car n) 's)
                    (cdr n)))))
   (letrec ((+
             (lambda (n m)
               (if (zero? n)
                   m
                   (add1 (+ (sub1 n) m))))))
   (letrec ((-
             (lambda (n m)
               (if (zero? m)
                   n
                   (sub1 (- n (sub1 m)))))))

   (letrec ((fib-aps ,fib-aps-skeleton))
     (list
       (fib-aps 'z ',ACC1 ',ACC2)
       (fib-aps '(s . z) ',ACC1 ',ACC2)
       (fib-aps '(s s . z) ',ACC1 ',ACC2)
       (fib-aps '(s s s . z) ',ACC1 ',ACC2)
       (fib-aps '(s s s s . z) ',ACC1 ',ACC2)
       (fib-aps '(s s s s s . z) ',ACC1 ',ACC2))
     )))))))

(record-bench 'run-staged 'peano-synth-fib-aps-1)
(time-test
  (run-staged #f (fib-acc ACC1 ACC2)
    (== `(lambda (n a1 a2)
           (if (zero? n)
               a1
               (if (zero? (sub1 n))
                   a2
                   (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))
        fib-acc)
    (evalo-staged
     (peano-synth-fib-aps fib-acc ACC1 ACC2)
     '(z
       (s . z)
       (s . z)
       (s s . z)
       (s s s . z)                   
       (s s s s s . z))))
  '(((lambda (n a1 a2)
       (if (zero? n)
           a1
           (if (zero? (sub1 n))
               a2
               (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))
     z
     (s . z))))

#|
;;;  Seems super slow---didn't return after a minute or so
(record-bench 'unstaged 'peano-synth-fib-aps-1)
(time-test
  (run #f (fib-acc ACC1 ACC2)
    (== `(lambda (n a1 a2)
           (if (zero? n)
               a1
               (if (zero? (sub1 n))
                   a2
                   (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))
        fib-acc)
    (evalo-unstaged
     (peano-synth-fib-aps fib-acc ACC1 ACC2)
     '(z
       (s . z)
       (s . z)
       (s s . z)
       (s s s . z)                   
       (s s s s s . z))))
  '(((lambda (n a1 a2)
       (if (zero? n)
           a1
           (if (zero? (sub1 n))
               a2
               (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))
     z
     (s . z))))
|#


;;  seems very slow, even with the symbolo hint
#|
(record-bench 'run-staged 'peano-synth-fib-aps-2)
(time-test
  (run-staged #f (fib-acc ACC1 ACC2)
    (fresh (A B)
      (symbolo B)
      (== `(lambda (n a1 a2)
             (if (zero? n)
                 a1
                 (if (zero? (sub1 n))
                     ,B
                     (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))
          fib-acc))
    (evalo-staged
     (peano-synth-fib-aps fib-acc ACC1 ACC2)
     '(z
       (s . z)
       (s . z)
       (s s . z)
       (s s s . z)                   
       (s s s s s . z))))
  '(((lambda (n a1 a2)
       (if (zero? n)
           a1
           (if (zero? (sub1 n))
               a2
               (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))
     z
     (s . z))))
|#


(record-bench 'run-staged 'peano-synth-fib-aps-3)
(time-test
  (run-staged #f (fib-acc)
    (== `(lambda (n a1 a2)
           (if (zero? n)
               a1
               (if (zero? (sub1 n))
                   a2
                   (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))
        fib-acc)
    (evalo-staged
     (peano-synth-fib-aps fib-acc 'z '(s . z))
     '(z
       (s . z)
       (s . z)
       (s s . z)
       (s s s . z)                   
       (s s s s s . z))))
  '((lambda (n a1 a2)
      (if (zero? n)
          a1
          (if (zero? (sub1 n))
              a2
              (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))))


(record-bench 'unstaged 'peano-synth-fib-aps-3)
(time-test
  (run #f (fib-acc)
    (== `(lambda (n a1 a2)
           (if (zero? n)
               a1
               (if (zero? (sub1 n))
                   a2
                   (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))
        fib-acc)
    (evalo-unstaged
     (peano-synth-fib-aps fib-acc 'z '(s . z))
     '(z
       (s . z)
       (s . z)
       (s s . z)
       (s s s . z)                   
       (s s s s s . z))))
  '((lambda (n a1 a2)
      (if (zero? n)
          a1
          (if (zero? (sub1 n))
              a2
              (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))))







(define (peano-fib query)
  `(letrec ((zero?
             (lambda (n)
               (equal? 'z n))))

   (letrec ((add1
             (lambda (n)
               (cons 's n))))
   (letrec ((sub1
             (lambda (n)
               (and (equal? (car n) 's)
                    (cdr n)))))
   (letrec ((+
             (lambda (n m)
               (if (zero? n)
                   m
                   (add1 (+ (sub1 n) m))))))
   (letrec ((-
             (lambda (n m)
               (if (zero? m)
                   n
                   (sub1 (- n (sub1 m)))))))

   (letrec ((fib-aps
             (lambda (n a1 a2)
               (if (zero? n)
                   a1
                   (if (zero? (sub1 n))
                       a2
                       (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))))
     ,query
     )))))))

(eval (peano-fib `(fib-aps '(s s s s s s . z) 'z '(s . z))))

(record-bench 'run-staged 'peano-fib-1)
(time-test
  (run-staged #f (v)
    (evalo-staged
     (peano-fib `(fib-aps '(s s s s s s . z) 'z '(s . z)))
     v))
  '((s s s s s s s s . z)))

(record-bench 'unstaged 'peano-fib-1)
(time-test
  (run* (v)
    (evalo-unstaged
     (peano-fib `(fib-aps '(s s s s s s . z) 'z '(s . z)))
     v))
  '((s s s s s s s s . z)))


(record-bench 'run-staged 'peano-fib-2)
(time-test
  (run-staged 1 (q)
    (evalo-staged
     (peano-fib `(fib-aps ',q 'z '(s . z)))
     '(s s s s s s s s . z)))
  '((s s s s s s . z)))

(record-bench 'unstaged 'peano-fib-2)
(time-test
  (run 1 (q)
    (evalo-unstaged
     (peano-fib `(fib-aps ',q 'z '(s . z)))
     '(s s s s s s s s . z)))
  '((s s s s s s . z)))


(record-bench 'run-staged 'peano-fib-3)
(time-test
  (run-staged 1 (q)
    (evalo-staged
     (peano-fib `(fib-aps ',q 'z '(s . z)))
     '(s s s s s s s s s s s s s . z)))
  '((s s s s s s s . z)))

(record-bench 'unstaged 'peano-fib-3)
(time-test
  (run 1 (q)
    (evalo-unstaged
     (peano-fib `(fib-aps ',q 'z '(s . z)))
     '(s s s s s s s s s s s s s . z)))
  '((s s s s s s s . z)))


(record-bench 'run-staged 'peano-fib-4)
(time-test
  (run-staged 5 (q)
    (evalo-staged
     (peano-fib `(fib-aps ,q 'z '(s . z)))
     '(s s s s s s s s s s s s s . z)))
  '('(s s s s s s s . z)
    ((letrec ([_.0 (lambda _.1 _.2)]) '(s s s s s s s . z))
     $$
     (=/= ((_.0 quote)))
     (sym _.1))
    ((match _.0 [_.0 '(s s s s s s s . z)] . _.1) $$ (num _.0))
    (and '(s s s s s s s . z))
    ((letrec ([_.0 (lambda () _.1)]) '(s s s s s s s . z))
     $$
     (=/= ((_.0 quote))))))

(record-bench 'unstaged 'peano-fib-4)
(time-test
  (run 5 (q)
    (evalo-unstaged
     (peano-fib `(fib-aps ,q 'z '(s . z)))
     '(s s s s s s s s s s s s s . z)))
  '('(s s s s s s s . z)
    ((letrec ([_.0 (lambda _.1 _.2)]) '(s s s s s s s . z))
     $$
     (=/= ((_.0 quote)))
     (sym _.1))
    ((match _.0 [_.0 '(s s s s s s s . z)] . _.1) $$ (num _.0))
    (and '(s s s s s s s . z))
    ((letrec ([_.0 (lambda () _.1)]) '(s s s s s s s . z))
     $$
     (=/= ((_.0 quote))))))




