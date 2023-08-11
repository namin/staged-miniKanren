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


;; Broader conclusion from this example:

;; Because at runtime when any branch succeeds, we get that answer from a run 1,
;; rather than just diverging... this means that staging always has to terminate
;; if there's a chance of any branch in the program succeeding at runtime... which
;; is presumably all the cases we care about.

;; So, every staged program to be correct needs to come with a termination argument.

;; The termination argument that we use for a staged interpreter with fallback and
;; gather is... feasible but subtle. Not the most usable, and it's only really feasible
;; because the staging time portion is just a simple, structurally recursive function.
;; If we were staging something where the staging-time portion is a terminating
;; function but that requires a more complex termination argument than structural
;; recursion, or where the staging-time portion is a relation in multiple arguments,
;; that may not work with the tools we have.

;; Are there other designs for a staged miniKanren where proving / reasoning about
;; termination would be much easier?
;;
;; Candidates...
;;
;; -- condg --
;;
;; How do you argue termination with `condg`?
;;
;; Possibilities with a interpreter implemented with condg:
;;  1. Everything fails; terminate.
;;  2. The guards (which should be finite computations) of multiple clauses succeed.
;;      fallback and terminate.
;;  3. One guard succeeds. We commit to that clause and evaluate the recursive calls.
;;     Thus termination of the overall call depends on termination of these recursions
;;     in this one clause.
;;         What argument do we use that these will terminate?
;;              Structural recursion again. Eventually we'll exhaust the ground syntax
;;              and fallback with case 2 above, or we'll hit a base case of the
;;              interpreter.
;;
;; So the argument is again structural recursion. Why is it easier to reason about
;; with condg than with fallback?
;;   The decision about whether we are going to fallback depends only on the
;;   (always-terminating) guards and not on the recursive bodies.
;;
;;   This is at least easier to think about.
;;
;; Does it change what we can easily express? Or do we have to follow basically the
;; same rules of making staging-time computation be limited to act as a structurally
;; recursive function with a base case, with other computation
;; deferred to runtime, and where each call only examines one argument at a time.
;;    Constraints between arguments can lead to failure; when that failure closes
;;    off all base cases but not all recursive cases then the termination argument
;;    for the fallback search does not hold.
;;         Is this a problem for the termination argument for the condg search?
;;               Well, in case 3 we still need a base case. We could still end up
;;               in an infinite recursion where all base cases fail but a recursive
;;               case appears possible.

;; Concretely, an interpreter with variable reference and `car`, where the intitial
;; environment has some lists to destruct. The only base cases is variable reference.
;; In an empty initial enviornment or when the expected value doesn't match the
;; environment value this case can fail.
#;(defrel/multistage/condg-fallback (eval-var-car e env v)
    ([]
     [(symbolo e)
      (lookupo e env v)]
     [])
    ([e1 v1 d]
     [(== e `(car ,e1))
      (== v1 `(,v . ,d))]
     [(evalo e1 env v1)]))
;; So this query could diverge at staging time:
;; (eval-var-car e '() 5)

;; Okay, making this interpeter always terminate. What does it take?
;; Just doing a later on the values isn't enough because lookup always fails
;; in an empty environment.
#;(defrel/multistage/condg-fallback (eval-var-car2 e env v)
    ([v2]
     [(symbolo e)
      (later (== v v2))
      (lookupo e env v2)]
     [])
    ([e1 v1 d]
     [(== e `(car ,e1))
      (later (== v1 `(,v . ,d)))]
     [(eval-var-car2 e1 env v1)]))

;; What about putting lookupo in the body, but no laters?
;; Then multiple guards succeed and we fall back.
#;(defrel/multistage/condg-fallback (eval-var-car3 e env v)
    ([v2]
     [(symbolo e)
      (== v v2)]
     [(lookupo e env v)])
    ([e1 v1 d]
     [(== e `(car ,e1))
      (== v1 `(,v . ,d))]
     [(eval-var-car3 e1 env v1)]))

;; Done with fallbacks, the symbol case isn't a reliable base case to ensure
;; termination.
(defrel/multistage/fallback (eval-var-car4 e env v)
  (conde
    ((fresh (v2)
       (symbolo e)
       (== v v2)
       (lookupo e env v)))
    ((fresh (e1 v1 d)
       (== e `(car ,e1))
       (== v1 `(,v . ,d))
       (eval-var-car4 e1 env v1)))))

(defrel/multistage/fallback (lookupo x env v)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      [(== x y) (== v b)]
      [(=/= x y) (lookupo x rest v)])))

;; So, this diverges!
#;(run 1 (e) (staged (eval-var-car4 e '() 5)))

;; And, constructing the case where runtime does terminate, using `gather`...
(defrel/multistage/fallback (eval-var-car5 e env v)
  (conde
    ((fresh (v2)
       (symbolo e)
       (later (== v v2))
       (lookupo e env v)))
    ((fresh (e1 v1 d)
       (== e `(car ,e1))
       (later (== v1 `(,v . ,d)))
       (eval-var-car5 e1 env v1)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (gather
        (conde
          ((eval-var-car5 e1 env v))
          ((eval-var-car5 e2 env v))))))))

(test
 (run 1 (e)
   (fresh ()
     (absento 'x e)
     (eval-var-car5 `(amb x ,e) '((x . 5)) 5)))
 '((_.0 $$ (absento (x _.0)))))

;; Diverges:
#;(run 1 (e)
    (staged
     (fresh ()
       (absento 'x e)
       (eval-var-car5 `(amb x ,e) '((x . 5)) 5))))

;; There isn't an obvious way to modify the interpreter to only do
;; the lookup after committing to the symbol case, while still doing the lookup at
;; staging-time.

;; Well, there is a way, but it's tricky:
(defrel/multistage/fallback (eval-var-car6 e env v)
  (conde
    ((fresh (v2)
       (symbolo e)
       (later (== v v2))
       (lookupo e env v)))
    ;; this is the added clause, to provide a base case
    ;; when `e` is a symbol but is not in the `env`
    ((symbolo e)
     (not-in-envo e env)
     (later fail))
    ((fresh (e1 v1 d)
       (== e `(car ,e1))
       (later (== v1 `(,v . ,d)))
       (eval-var-car6 e1 env v1)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (gather
        (conde
          ((eval-var-car6 e1 env v))
          ((eval-var-car6 e2 env v))))))))
(defrel/multistage (not-in-envo x env)
  (conde
    ((== '() env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))


(test
 (run 1 (e)
   (fresh ()
     (absento 'x e)
     (eval-var-car6 `(amb x ,e) '((x . 5)) 5)))
 '((_.0 $$ (absento (x _.0)))))

;; now this terminates
(test
 (run 1 (e)
   (staged
    (fresh ()
      (absento 'x e)
      (eval-var-car6 `(amb x ,e) '((x . 5)) 5))))
 '((_.0 $$ (absento (x _.0)))))


;; `condg` is more workable for this case because we can make it fall back as
;; "potentially non-deterministic" even in the case where if we evaluated further
;; we would find that only one branch, or even no branch at all succeeds.
;;
;; An "approximate" fallback. Whereas the only way to get `fallback` to fallback
;; is to actually defer stuff to runtime.
;;
;; It feels like we could have a kind of "cut" analogy that says "yeah, assume this
;; could work if you get this far and count it".


;; So as a conclusion, the fallback approach has additional problems for relations
;; without sufficiently simple base cases. You have to prove that there will always
;; be a base case that can succeed, and sometimes that may not be true.

;; The tradeoff might still be worth it, though. The syntactic simplicity of
;; transforming an interpreter is really nice!

;; Either a combination of fallback with quasiquoted conde rather than gather,
;; or gather with condg could escape this problem perhaps.