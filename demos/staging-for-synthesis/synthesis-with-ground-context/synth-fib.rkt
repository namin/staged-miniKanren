#lang racket

(require "../../../all.rkt")

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

(defrel (peano-synth-fib-aps-staged4 A B C ACC1 ACC2 fib-acc)
  (time-staged
   (fresh ()
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

(time
 (run 1 (A B C ACC1 ACC2)
   (fresh (whole-fn)
     (peano-synth-fib-aps-staged4 A B C ACC1 ACC2 whole-fn))))
(generated-code)


;; The unstaged version of this query doesn't complete in 5 minutes (from our benchmarks)
