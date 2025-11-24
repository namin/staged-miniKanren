#lang racket

(require "../../all.rkt")

(define-term-syntax-rule (prover body)
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
       ,body)))

(defrel (proof-unstaged prf)
  (evalo-unstaged
   (prover `(proof? ',prf))
   #t))

(defrel (proof-staged prf)
  (time-staged
   (evalo-staged
    (prover `(proof? ',prf))
    #t)))
#;(generated-code)

(time
 (run 1 (prf)
   (fresh (body)
     (== prf `(((A => B) => ((B => C) => (A => C))) () . ,body))
     (proof-unstaged prf))))

(time
 (run 1 (prf)
   (fresh (body)
     (== prf `(((A => B) => ((B => C) => (A => C))) () . ,body))
     (proof-staged prf))))