#lang racket/base

(require "../../all.rkt")

;; Adapted from https://github.com/k-tsushima/Shin-Barliman/blob/master/transformations/peano.scm
;; and https://github.com/k-tsushima/Shin-Barliman/blob/master/transformations/peano-relational.scm

;; Currently not synthesizing
(define-term-syntax-rule (peano-synth-fib-direct fib-skeleton)
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

(defrel (peano-synth-fib-direct-staged fib-direct)
  (staged
    (fresh ()
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
         (s s s s s . z))))))


(test
 (run* (fib-direct)
   (peano-synth-fib-direct-staged fib-direct))
  '((lambda (n)
       (if (zero? n)
           'z
           (if (zero? (sub1 n))
               '(s . z)
               (+ (fib (sub1 n)) (fib (sub1 (sub1 n)))))))))

(test
  (run* (fib-direct)
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

(define-term-syntax-rule (peano-synth-fib-aps fib-aps-skeleton ACC1 ACC2)
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

(define-term-syntax-rule (peano-synth-fib-aps-with-an-extra-example fib-aps-skeleton ACC1 ACC2)
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
       (fib-aps '(s s s s s . z) ',ACC1 ',ACC2)
       (fib-aps '(s s s s s s . z) ',ACC1 ',ACC2))
     )))))))


(defrel (peano-synth-fib-aps-staged1 fib-acc ACC1 ACC2)
  (staged
    (fresh ()
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
         (s s s s s . z))))))


(test
 (run* (fib-acc)
   (peano-synth-fib-aps-staged1 fib-acc 'z '(s . z)))
  '((lambda (n a1 a2)
      (if (zero? n)
          a1
          (if (zero? (sub1 n))
              a2
              (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))))


(test
  (run* (fib-acc)
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

;; Synthesize the accumulators
(test
 (run* (fib-acc ACC1 ACC2)
   (peano-synth-fib-aps-staged1 fib-acc ACC1 ACC2))
   '(((lambda (n a1 a2)
		(if (zero? n)
			a1
			(if (zero? (sub1 n))
				a2
				(fib-aps (- n '(s . z)) a2 (+ a1 a2)))))
	  z
	  (s . z))))

(test
  (run* (fib-acc ACC1 ACC2)
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


(record-bench 'synth/ground-context 'staging 'synth-peano-fib)
(defrel (synth-peano-fib-base-case e ACC1 ACC2)
  (time-staged
    (fresh (fib-acc)
      (== `(lambda (n a1 a2)
             (if (zero? n)
                 a1
                 (if (zero? (sub1 n))
                     ,e
                     (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))
          fib-acc)
      (evalo-staged
       (peano-synth-fib-aps fib-acc ACC1 ACC2)
       '(z
         (s . z)
         (s . z)
         (s s . z)
         (s s s . z)
         (s s s s s . z))))))

;; a small-one-hole version synthesis w/synth accs; small enough for unstaged to do it
(record-bench 'synth/ground-context 'staged 'synth-peano-fib)
(time-test
  (run 1 (e ACC1 ACC2)
    (synth-peano-fib-base-case
     e
     ACC1
     ACC2))
  '((a2 z (s . z))))

(defrel (synth-peano-fib-base-case-unstaged e ACC1 ACC2)
    (fresh (fib-acc)
      (== `(lambda (n a1 a2)
             (if (zero? n)
                 a1
                 (if (zero? (sub1 n))
                     ,e
                     (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))
          fib-acc)
      (evalo-unstaged
       (peano-synth-fib-aps fib-acc ACC1 ACC2)
       '(z
         (s . z)
         (s . z)
         (s s . z)
         (s s s . z)
         (s s s s s . z)))))

(record-bench 'synth/ground-context 'unstaged 'synth-peano-fib #:description "synth fib base case from examples")
(time-test
  (run 1 (e ACC1 ACC2)
    (synth-peano-fib-base-case-unstaged
     e
     ACC1
     ACC2))
  '((a2 z (s . z))))

(defrel (peano-synth-fib-aps-stepo step1 step2 ACC1 ACC2 result)
  (staged
	(fresh (fib-acc)
	  (== `(lambda (n a1 a2)
			 (if (zero? n)
				 a1
				 (if (zero? (sub1 n))
					 a2
					 (fib-aps (- n '(s . z)) ,step1 ,step2))))
		  fib-acc)
	  (evalo-staged
		(peano-synth-fib-aps fib-acc ACC1 ACC2)
		result))))

(test
  (run 1 (step1 step2 ACC1 ACC2)
    (peano-synth-fib-aps-stepo
      step1
      step2
      ACC1
      ACC2
      '(z
        (s . z)
        (s . z)
        (s s . z)
        (s s s . z)
        (s s s s s . z))))
  '((a2
     (+ a1 a2)
     z
     (s . z))))

(defrel (peano-synth-fib-aps-stepo-unstaged step1 step2 ACC1 ACC2 result)
  (fresh (fib-acc)
	(== `(lambda (n a1 a2)
		   (if (zero? n)
			   a1
			   (if (zero? (sub1 n))
				   a2
				   (fib-aps (- n '(s . z)) ,step1 ,step2))))
		fib-acc)
	(evalo-unstaged
	  (peano-synth-fib-aps fib-acc ACC1 ACC2)
	  result)))

;; Test fails rn; need extra examples to force timeout or general correct answer
;; (test
;;   (run 1 (step1 step2 ACC1 ACC2)
;;     (peano-synth-fib-aps-stepo-unstaged
;;       step1
;;       step2
;;       ACC1
;;       ACC2
;;       '(z
;;         (s . z)
;;         (s . z)
;;         (s s . z)
;;         (s s s . z)
;;         (s s s s s . z))))
;;   'timeout)



(defrel (peano-synth-aps-staged3 fib-acc ACC1 ACC2)
  (staged
    (fresh (A B)
      (== `(lambda (n a1 a2)
             (if (zero? n)
                 a1
                 (if (zero? (sub1 n))
                     a2
                     (fib-aps (- n '(s . z)) ,A ,B))))
          fib-acc)
      (evalo-staged
       (peano-synth-fib-aps-with-an-extra-example fib-acc ACC1 ACC2)
       '(z
         (s . z)
         (s . z)
         (s s . z)
         (s s s . z)
         (s s s s s . z)
         (s s s s s s s s . z))))))


(test
 (run 1 (fib-acc ACC1 ACC2)
   (peano-synth-aps-staged3 fib-acc ACC1 ACC2))
 '(((lambda (n a1 a2)
	  (if (zero? n)
		  a1
		  (if (zero? (sub1 n))
			  a2
			  (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))
	z
	(s . z))))

;; Test fails rn; need extra examples to force timeout or general correct answer
;; (test
;;   (run 1 (fib-acc ACC1 ACC2)
;;     (fresh (A B)
;;       (== `(lambda (n a1 a2)
;;              (if (zero? n)
;;                  a1
;;                  (if (zero? (sub1 n))
;;                      a2
;;                      (fib-aps (- n '(s . z)) ,A ,B))))
;;           fib-acc))
;;     (evalo-unstaged
;;      (peano-synth-fib-aps-with-an-extra-example fib-acc ACC1 ACC2)
;;      '(z
;;        (s . z)
;;        (s . z)
;;        (s s . z)
;;        (s s s . z)
;;        (s s s s s . z)
;;        (s s s s s s s s . z))))
;;   'timeout)

(record-bench 'synth/ground-context 'staging 'fib-synth-3-holes)
(defrel (peano-synth-fib-aps-staged4 fib-acc ACC1 ACC2)
  (time-staged
    (fresh (A B C)
      (== `(lambda (n a1 a2)
             (if (zero? n)
                 ,A
                 (if (zero? (sub1 n))
                     a2
                     (fib-aps (- n '(s . z)) ,B ,C))))
          fib-acc)
      (absento 'match fib-acc)
      (evalo-staged
       (peano-synth-fib-aps-with-an-extra-example fib-acc ACC1 ACC2)
       '(z
         (s . z)
         (s . z)
         (s s . z)
         (s s s . z)
         (s s s s s . z)
         (s s s s s s s s . z))))))


(record-bench 'synth/ground-context 'staged 'fib-synth-3-holes)
(time-test
 (run 1 (fib-acc ACC1 ACC2)
   (peano-synth-fib-aps-staged4 fib-acc ACC1 ACC2))
   '(((lambda (n a1 a2)
		(if (zero? n)
			a1
			(if (zero? (sub1 n))
				a2
				(fib-aps (- n '(s . z)) a2 (+ a1 a2)))))
	  z
	  (s . z))))

(record-bench 'synth/ground-context 'unstaged 'fib-synth-3-holes #:description "synthesizes the accumulators and three holes in the program")
(time-test
  (run 1 (fib-acc ACC1 ACC2)
    (fresh (A B C)
      (== `(lambda (n a1 a2)
             (if (zero? n)
                 ,A
                 (if (zero? (sub1 n))
                     a2
                     (fib-aps (- n '(s . z)) ,B ,C))))
          fib-acc))
    (evalo-unstaged
     (peano-synth-fib-aps-with-an-extra-example fib-acc ACC1 ACC2)
     '(z
       (s . z)
       (s . z)
       (s s . z)
       (s s s . z)
       (s s s s s . z)
       (s s s s s s s s . z))))
  'timeout)


(define-term-syntax-rule (peano-fib body)
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
     ,body
     )))))))

;;(eval (peano-fib `(fib-aps '(s s s s s s . z) 'z '(s . z))))

(record-bench 'eval/program 'staging 'peano-fib)
(defrel (fib-apso n a1 a2 result)
  (time-staged
   (evalo-staged
    (peano-fib `(fib-aps ',n ',a1 ',a2))
    result)))

(test
 (run* (v)
   (fib-apso '(s s s s s s . z) 'z '(s . z) v))
  '((s s s s s s s s . z)))

(test
  (run* (v)
    (evalo-unstaged
     (peano-fib `(fib-aps '(s s s s s s . z) 'z '(s . z)))
     v))
  '((s s s s s s s s . z)))


(test
  (run 1 (q)
    (fib-apso q 'z '(s . z)
              '(s s s s s s s s . z)))
  '((s s s s s s . z)))

(test
  (run 1 (q)
    (evalo-unstaged
     (peano-fib `(fib-aps ',q 'z '(s . z)))
     '(s s s s s s s s . z)))
  '((s s s s s s . z)))

;; running backwards synthesize an input value that makes the peano-fib function produce the expected value.
(record-bench 'eval/program 'staged 'peano-fib)
(time-test
  (run 1 (q)
    (fib-apso q 'z '(s . z)
              '(s s s s s s s s s s s s s . z)))
  '((s s s s s s s . z)))

(record-bench 'eval/program 'unstaged 'peano-fib #:description "synthesize a value that makes the peano-fib function produce the right output.")
(time-test
  (run 1 (q)
    (evalo-unstaged
     (peano-fib `(fib-aps ',q 'z '(s . z)))
     '(s s s s s s s s s s s s s . z)))
  '((s s s s s s s . z)))

(record-bench 'eval/program 'staging 'peano-fib-expressions)
(defrel (peano-fib4 q)
  (time-staged
    (evalo-staged
     (peano-fib `(fib-aps ,q 'z '(s . z)))
     '(s s s s s s s s s s s s s . z))))


;; instead of synthesizing a value, we instead synth arb expressions that have the appropriate value
(record-bench  'eval/program 'staged 'peano-fib-expressions)
(time-test
 (run 5 (q)
   (peano-fib4 q))
 '('(s s s s s s s . z)
   ((letrec ((_.0 (lambda _.1 _.2))) '(s s s s s s s . z))
    $$
    (=/= ((_.0 quote))))
   (((lambda _.0 '(s s s s s s s . z))) $$ (=/= ((_.0 quote))) (sym _.0))
   (((lambda _.0 '(s s s s s s s . z)) _.1)
    $$
    (=/= ((_.0 quote)))
    (num _.1)
    (sym _.0))
   ((lambda () '(s s s s s s s . z)))))

(record-bench 'eval/program 'unstaged 'peano-fib-expressions #:description "synthesize an expression that makes the peano-fib function produce the right output.")
(time-test
  (run 5 (q)
    (evalo-unstaged
     (peano-fib `(fib-aps ,q 'z '(s . z)))
     '(s s s s s s s s s s s s s . z)))
  '('(s s s s s s s . z)
    ((letrec ((_.0 (lambda _.1 _.2))) '(s s s s s s s . z))
     $$
     (=/= ((_.0 quote))))
    ((match _.0 (_.0 '(s s s s s s s . z)) . _.1) $$ (num _.0))
    (and '(s s s s s s s . z))
    ((match _.0 (_.1 '(s s s s s s s . z)) . _.2)
     $$
     (=/= ((_.1 quote)))
     (num _.0)
     (sym _.1))))
