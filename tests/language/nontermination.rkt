#lang racket/base

(require "../../main.rkt"
         "../../test-check.rkt")

(defrel/multistage/fallback (evalo e v)
  (conde
    ((numbero e)
     (== e v))
    ((fresh (e1 v1 e2 v2)
       (== e `(cons ,e1 ,e2))
       (== v `(,v1 . ,v2))
       (evalo e1 v1)
       (evalo e2 v2)))
    ((fresh (e1 v1 d)
       (== e `(car ,e1))
       (== v1 `(,v . ,d))
       (evalo e1 v1)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (gather
        (conde
          ((evalo e1 v))
          ((evalo e2 v))))))))

(test
 (run 1 (v)
   (staged (evalo '(car (cons 1 2))
                  v)))
 '(1))

;; Try to generate an expression that produces a symbol; no case in the
;; interpreter does this. All the cases for introduction forms fail. There
;; is exactly one elimination form; that case recurs and we loop forever.
;; Diverges:
#;(run 0 (e) (staged (evalo e 'x)))

;; What do we think of this? We may not like that staging this interpreter
;; can ever fail to terminate. On that basis alone we may prefer to `later`
;; the value unifications to make it underconstrained and fallback in this case.

;; However, we might think we are okay with staging diverging when the runtime
;; query will inevitably diverge. In that case, the example so far might seem
;; okay. The runtime version here diverges too:
;; Diverges:
#;(run 1 (e) (evalo e 'x))

;; I was surprised that adding amb didn't make the above staged query terminate; I
;; thought having two elimination forms to choose from would lead to fallback. But
;; since the recursions for both of these cases still can't terminate or fallback,
;; we've still got divergence.

;; But, the problem gets worse! `gather` effectively does a run* at staging time
;; on its subgoal. In the case that some branch of the subgoal can succeed but
;; another branch diverges, this can lead staging to diverge when the runtime query
;; would succeed for a run 1.

;; An example:
(test
 (run 1 (e)
   (fresh ()
     (absento 1 e)
     (evalo `(amb 1 ,e)
            1)))
 '((_.0 $$ (absento (1 _.0)))))
;; Diverges:
#;(run 1 (e)
  (staged
   (fresh ()
     (absento 1 e)
     (evalo `(amb 1 ,e)
            1))))

;; To work around this problem, we delay all unifications with the
;; value until runtime with `later`. As a result, the following are
;; the possible situations for an evalo-later call:
;;
;; 1) There is a syntax error in the top syntactic form. All cases fail,
;;    so staging fails.
;; 2) The top syntactic form is not sufficiently specified to select a
;;    clause. Multiple clauses appear possible based on the immediate
;;    constraints.
;;       But fallback will only occur when we find the clause is completely
;;       satisfiable, including recursions! So how does this bottom out?
;;          It works because when we recur sufficiently such that the expression
;;       is a fresh variable, it can match the base case of (numbero e) in the
;;       interpreter. The fallback search is clever enough to propagate the fact
;;       that there is at least one solution to the recursion upwards, allowing
;;       the outer call to fall back.
;; 3) Only one appears syntactically possible. All immediate constraints not
;;    relating to the syntax are always in `later`, so they will not cause failure.
;;    Structural recursion on the syntax ensures that either recursion terminates as
;;    we reach the leaves of a known expression, or the expression is eventually
;;    fully unknown, and we again bottom out at the base case of (numbero e).

;; Conditions required here:
;;
;; Nada's initial formulation: you have to have a base case, and unifying with the
;; value at staging time may make the base case fail.

(defrel/multistage/fallback (evalo-later e v)
  (conde
    ((numbero e)
     (later (== e v)))
    ((fresh (e1 v1 e2 v2)
       (== e `(cons ,e1 ,e2))
       (later (== v `(,v1 . ,v2)))
       (evalo-later e1 v1)
       (evalo-later e2 v2)))
    ((fresh (e1 v1 d)
       (== e `(car ,e1))
       (later (== v1 `(,v . ,d)))
       (evalo-later e1 v1)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (gather
        (conde
          ((evalo-later e1 v))
          ((evalo-later e2 v))))))))

(test
 (run 1 (e)
   (fresh ()
     (absento 1 e)
     (evalo-later `(amb 1 ,e)
                  1)))
 '((_.0 $$ (absento (1 _.0)))))

(test
 (run 1 (e)
   (staged
    (fresh ()
      (absento 1 e)
      (evalo-later `(amb 1 ,e)
                   1))))
 '((_.0 $$ (absento (1 _.0)))))
