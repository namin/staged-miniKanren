#lang racket/base

(require "../all.rkt")
(require "pldi/appendo.rkt")
(require "pldi/quasiquine.rkt")

;; turn function into relation
;; turn proof checker into theorem prover

(defrel (proofo proof truth)
  (staged
   (evalo-staged
    `(letrec ([member?
               (lambda (x ls)
                 (if (null? ls) #f
                     (if (equal? (car ls) x) #t
                         (member? x (cdr ls)))))])
       (letrec ([proof?
                 (lambda (proof)
                   (match proof
                     [`(,A ,assms assumption ()) (member? A assms)]
                     [`(,B ,assms modus-ponens
                           (((,A => ,B) ,assms ,r1 ,ants1)
                            (,A ,assms ,r2 ,ants2)))
                      (and (proof? (list (list A '=> B) assms r1 ants1))
                           (proof? (list A assms r2 ants2)))]
                     [`((,A => ,B) ,assms conditional
                        ((,B (,A . ,assms) ,rule ,ants)))
                      (proof? (list B (cons A assms) rule ants))]))])
         (proof? ',proof)))
    truth)))

(time
 (run 1 (prf)
   (fresh (body)
     (== prf `(C (A (A => B) (B => C)) . ,body))
     (proofo prf #t))))

(time
 (run 1 (prf)
   (fresh (body)
     (== prf `(H (A (A => B) (B => C) (C => D) (D => E) (E => F) (F => G) (G => H)) . ,body))
     (proofo prf #t))))

;; quasi-quote interpreter
(time
 (run 1 (q)
   (absento 'error q) ;; without this constraint, 'error is a quine! (because the empty env returns 'error)
   (absento 'struct q)
   (quasi-quine-evalo-staged q q)))

;; synthesis with context
(defrel (peano-synth-fib-acc-stepo step1 step2 ACC1 ACC2)
  (staged
   (evalo-staged
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
               (letrec ((fib-acc
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-acc (- n '(s . z)) ,step1 ,step2))))))
                 (list
                  (fib-acc 'z ',ACC1 ',ACC2)
                  (fib-acc '(s . z) ',ACC1 ',ACC2)
                  (fib-acc '(s s . z) ',ACC1 ',ACC2)
                  (fib-acc '(s s s . z) ',ACC1 ',ACC2)
                  (fib-acc '(s s s s . z) ',ACC1 ',ACC2)
                  (fib-acc '(s s s s s . z) ',ACC1 ',ACC2))
                 ))))))
    '(z
      (s . z)
      (s . z)
      (s s . z)
      (s s s . z)
      (s s s s s . z)))))

(time
 (run 1 (step1 step2 ACC1 ACC2)
   (peano-synth-fib-acc-stepo
    step1
    step2
    ACC1
    ACC2))
)
