#lang racket/base

(require "../../all.rkt" racket/list plot)

(provide proof-chart)

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

(defrel/staged (proof-staged prf)
  (evalo-staged
   (prover `(proof? ',prf))
   #t))

(defrel (proof-unstaged prf)
  (evalo-unstaged
   (prover `(proof? ',prf))
   #t))

(record-bench 'eval-eval 'staging 'proofo #:description "Checks the validity of proofs for implicational propositional calculus")
(defrel (proofo prf valid-proof?)
  (time-staged
   (evalo-staged
    (prover `(proof? ',prf))
    valid-proof?)))

(defrel (valid-proofo prf)
  (staged
   (evalo-staged
    (prover `(proof? ',prf))
    #t)))

(define (run-proofs)
  (define ex-proof1
    '((C (A (A => B) (B => C))
         modus-ponens
         (((B => C) (A (A => B) (B => C)) assumption ())
          (B (A (A => B) (B => C))
             modus-ponens
             (((A => B) (A (A => B) (B => C)) assumption ())
              (A (A (A => B) (B => C)) assumption ())))))))

  (record-bench 'eval-eval 'staged 'proofo 1)
  (time-test
    (run 1 (prf)
      (fresh (body)
        (== prf `(C (A (A => B) (B => C)) . ,body))
        (proofo prf #t)))
    ex-proof1)

  (record-bench 'eval-eval 'unstaged 'proofo 1 #:description "Synthesize a proof of C from assumptions as in \\cref{fig:proofo}")
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

  (record-bench 'eval-eval 'staged 'proofo 2)
  (time-test
    (run 1 (prf)
      (fresh (body)
        (== prf `(((A => B) => ((B => C) => (A => C))) () . ,body))
        (proofo prf #t)))
    ex-proof2)

  (record-bench 'eval-eval 'unstaged 'proofo 2 #:description "Synthesize a proof of \\((A \\Rightarrow B) \\Rightarrow ((B \\Rightarrow C) \\Rightarrow (A \\Rightarrow C))\\)" )
  (time-test
    (run 1 (prf)
      (fresh (body)
        (== prf `(((A => B) => ((B => C) => (A => C))) () . ,body))
        (proof-unstaged prf)))
    ex-proof2)

  (record-bench 'eval-eval 'staged 'proofo 3)
  (time-test
   (length
    (run 1 (prf)
      (fresh (body)
        (== prf `(((A => B) => ((B => C) => ((C => D) => ((D => E) => (A => E))))) () . ,body))
        (proofo prf #t))))
   1)

  (record-bench 'eval-eval 'unstaged 'proofo 3 #:description "Synthesize a proof of a longer chain of inferences")
  (time-test
   (length
	(run 1 (prf)
	  (fresh (body)
		(== prf `(((A => B) => ((B => C) => ((C => D) ((D => E)  => (A => E))))) () . ,body))
		 (proof-unstaged prf))))
	'timeout)
)

(run-proofs)

(define (implication-chain size premise conclusion)
  (let loop ([previous premise] [acc '()] [size size])
    (if (zero? size)
        (cons `(,previous => ,conclusion) acc)
        (let ([new (gensym)])
          (loop new (cons `(,previous => ,new) acc) (sub1 size))))))

(define (time-proof-of-size size)
  (define impl-chain (implication-chain size 'Start 'End))
  (define-syntax-rule (proof-time-of proof-rel)
    (let-values
        ([(_ __ wall-clock-time ___)
          (time-apply
           (lambda ()
             (run 1 (prf)
               (fresh (body)
                 (== prf `(End (Start . ,impl-chain) . ,body))
                 (proof-rel prf))))
           '())])
      wall-clock-time))

  (values (proof-time-of proof-unstaged) (proof-time-of valid-proofo)))

(define (proof-chart [sizes (in-range 1 5)] [filename #f])
  (define-values (unstaged staged)
    (for/lists (unstaged staged)
               ([size sizes])
      (printf "Timing proof of size ~a~%" size)
      (define-values (unstaged staged) (time-proof-of-size size))
      (values (list size unstaged) (list size staged))))
  (parameterize ([plot-x-ticks (fraction-ticks #:divisors '(1))])
    (plot (list (lines unstaged #:color 'red #:label "Unstaged")
                (lines staged #:color 'blue #:label "Staged"))
          #:x-label "Proof Size"
          #:y-label "Time (ms)"
          #:title "Unstaged/Staged Runtime vs Proof Size"
          #:out-file filename)))
