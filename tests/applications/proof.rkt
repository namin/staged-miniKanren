#lang racket/base

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
                    (proof? (list B (cons A assms) rule ants))]
                   ))])
       ,body)))

(defrel/staged (proof-staged prf)
  (evalo-staged
   (prover `(proof? ',prf))
   #t))

(defrel (proof-unstaged prf)
  (evalo-unstaged
   (prover `(proof? ',prf))
   #t))

(record-bench 'staging 'proofo)
(defrel (proofo prf b)
  (time-staged
   (evalo-staged
    (prover `(proof? ',prf))
    b)))

(define ex-proof1
  '((C (A (A => B) (B => C))
       modus-ponens
       (((B => C) (A (A => B) (B => C)) assumption ())
        (B (A (A => B) (B => C))
           modus-ponens
           (((A => B) (A (A => B) (B => C)) assumption ())
            (A (A (A => B) (B => C)) assumption ())))))))
(record-bench 'staged 'proofo 1)
(time-test
  (run 1 (prf)
    (fresh (body)
      (== prf `(C (A (A => B) (B => C)) . ,body))
      (proofo prf #t)))
  ex-proof1)

(record-bench 'run-staged 'proofo 1)
(time-test
 (run 1 (prf)
   (staged
    (fresh (body)
      (== prf `(C (A (A => B) (B => C)) . ,body))
      (proof-staged prf))))
  ex-proof1)

(record-bench 'unstaged 'proofo 1)
(time-test
  (run 1 (prf)
    (fresh (body)
      (== prf `(C (A (A => B) (B => C)) . ,body))
      (proof-unstaged prf)))
  ex-proof1)

(define ex-proof2
  '((((A => B) => ((B => C) => (A => C)))
     ()
     conditional
     ((((B => C) => (A => C))
       ((A => B))
       conditional
       (((A => C)
         ((B => C) (A => B))
         conditional
         ((C (A (B => C) (A => B))
             modus-ponens
             (((B => C) (A (B => C) (A => B)) assumption ())
              (B (A (B => C) (A => B))
                 modus-ponens
                 (((A => B) (A (B => C) (A => B)) assumption ())
                  (A (A (B => C) (A => B)) assumption ())))))))))))))

(record-bench 'staged 'proofo 2)
(time-test
  (run 1 (prf)
    (fresh (body)
      (== prf `(((A => B) => ((B => C) => (A => C))) () . ,body))
      (proofo prf #t)))
  ex-proof2)

(record-bench 'run-staged 'proofo 2)
(time-test
 (run 1 (prf)
   (staged
    (fresh (body)
      (== prf `(((A => B) => ((B => C) => (A => C))) () . ,body))
      (proof-staged prf))))
  ex-proof2)

(record-bench 'unstaged 'proofo 2)
(time-test
  (run 1 (prf)
    (fresh (body)
      (== prf `(((A => B) => ((B => C) => (A => C))) () . ,body))
      (proof-unstaged prf)))
  ex-proof2)

(record-bench 'staged 'proofo 3)
(time-test
 (length
  (run 1 (prf)
    (fresh (body)
      (== prf `(((A => B) => ((B => C) => ((C => D) => ((D => E) => (A => E))))) () . ,body))
      (proofo prf #t))))
  1)

(record-bench 'run-staged 'proofo 3)
(time-test
 (length
  (run 1 (prf)
    (staged
     (fresh (body)
       (== prf `(((A => B) => ((B => C) => ((C => D)  => ((D => E) => (A => E))))) () . ,body))
       (proof-staged prf)
       ))))
 1)

#| doesn't come back
(record-bench 'unstaged 'proofo 3)
(time-test
 (length
  (run 1 (prf)
    (fresh (body)
      (== prf `(((A => B) => ((B => C) => ((C => D) ((D => E)  => (A => E))))) () . ,body))
       (prover prf))))
  1)
|#
