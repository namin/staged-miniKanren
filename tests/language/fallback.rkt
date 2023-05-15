#lang racket/base

(require "../../main.rkt"
         "../../test-check.rkt")

;; When there's only one branch, produce that answer.
(test
 (run 1 (q)
      (staged
       (fallback
        (later (== q 'fallback))
        (conde
          ((== q 'single-branch) (later (== q 'single-branch)))))))
 '(single-branch))

;; When there are multiple successful branches, fall back.
(test
 (run 1 (q)
      (staged
       (fallback
        (later (== q 'fallback))
        (conde
          ((== q 'branch-1) (later (== q 'branch-1)))
          ((== q 'branch-2) (later (== q 'branch-2)))))))
 '(fallback))

;; When only one branch can succeed because of previous constraints, produce that answer.
(test
 (run 1 (q)
      (staged
       (fresh (x)
         (== x 1)
         (fallback
          (later (== q 'fallback))
          (conde
            ((== x 1) (later (== q 'branch-1)))
            ((== x 2) (later (== q 'branch-2))))))))
 '(branch-1))

;; Results can also be filtered after the branch but before returning from the fallback.
(test
 (run 1 (q)
   (staged
    (fallback
     (later (== q 'fallback))
     (fresh (x)
       (conde
         ((== x 1) (later (== q 'branch-1)))
         ((== x 2) (later (== q 'branch-2))))
       (== x 2)))))
 '(branch-2))

;; After exiting the fallback form, it is too late for new information to filter branches
;; and avoid fallback.
(test
 (run 1 (q)
      (staged
       (fresh (x)
         (fallback
          (later (== q 'fallback))
          (conde
            ((== x 1) (later (== q 'branch-1)))
            ((== x 2) (later (== q 'branch-2)))))
         (== x 2))))
 '(fallback))
;; TODO: is this a good thing? One could also imagine ensuring multiple branches are
;; successful all the way through to the halt continuation before committing to fall
;; back. But I think that would lead to diverging searches at staging time when you
;; want to defer the divergence to runtime.

;; Recognizing the need to fall back here is made tricky by the recursion nesting
;; fallback forms. For some fallback to fall back, it has to know that both its base
;; and recursive cases can succeed. But its recursive case contains a nested fallback
;; form with the same challenge! We can only successfully fall back if the inner fallback
;; reports out that it has a base-case branch that can succeed, thus allowing the outer
;; fallback form to know it has at least two solutions.
;;
;; Also notice that we still need interleaving search here! A depth-first-search would
;; just keep going deeper into the recursive case, because it is ordered first. It
;; would never discover the base-case successes that allow us to fall back.
(defrel/generator (membero x l log)
  (fallback
   (later (== log 'fallback))
   (fresh (first rest log-rest)
     (== `(,first . ,rest) l)
     (conde
       [(=/= x first)
        (== log `(recursive-case . ,log-rest))
        (membero x rest log-rest)]
       [(== x first)
        (== log 'base-case)]))))
(test
 (run 1 (q)
   (staged
    (fresh (l)
      (membero 'x `(y . ,l) q))))
 '((recursive-case . fallback)))

;; In this example the possibilities of q = 1 and q = 2 should be enough to
;; trigger the outer fallback, even though the `nevero` never terminates.
(defrel/generator (nevero)
  (conde
    [fail]
    [(nevero)]))
(test
 (run 1 (q)
   (staged
    (fallback
     (later (== q 'fallback-1))
     (conde
       [(== q 'branch-1)]
       [(fallback
         (later (== q 'fallback-2))
         (conde
           [(== q 'branch-2)]
           [(nevero)]))]))))
 '(fallback-1))

;; Here we must run the (== q 2) goal outside of the inner fallback in order to
;; discover that the outer fallback is nondeterministic. This means that the first
;; solution of the inner fallback must be tried with the success-k of the inner
;; fallback before we have committed to the decision of whether to fall back or not.
(test
 (run 1 (q)
   (staged
    (fallback
     (later (== q 'fallback-1))
     (conde
       [(== q 'branch-1)]
       [(fresh (x)
          (fallback
           (later (== q 'fallback-2))
           (conde
             [(== x 2) (== q 'branch-2)]
             [(nevero)]))
          (== x 2))]))))
 '(fallback-1))

;; Regression test: success of the first goal in a conjunct must not notify
;; success of the whole conjunction.
;;
;; In this example the first goal (== 1 1) in the first branch will
;; succeed, but the overall branch will fail. We don't want to notify-success
;; for this branch and thus end up falling back.
(test
 (run 1 (q)
      (staged
       (fresh (x)
         (== x 2)
         (fallback
          (later (== q 'fallback))
          (conde
            ((== 1 1) (== x 1) (later (== q 'branch-1)))
            ((== x 2) (later (== q 'branch-2))))))))
 '(branch-2))
;; But when the whole conjunction succeeds we do need the notify to make
;; it out.
(test
 (run 1 (q)
      (staged
       (fresh (x)
         (fallback
          (later (== q 'fallback))
          (conde
            ((== 1 1) (== x 1) (later (== q 'branch-1)))
            ((== x 2) (later (== q 'branch-2))))))))
 '(fallback))

;; Regression test: notify-success while evaluating a conjunct outside of and after
;; a fallback (that is, in the success continuation given to the fallback) should not be
;; counted by the fallback as indicating nondeterminism.
(test
 (run 1 (x)
      (staged
       (fresh ()
         (fallback
          (later (== x 'fallback))
          (later (== x 'only-answer)))
         (== 1 1))))
 '(only-answer))


;; Regression test: success of the fallback goal should not cause an outer
;; fallback form to double-count.
;;
;; Here both branches of the inner disjunction will succeed. The first such
;; success will cause the inner fallback form to notify success outward. Subsequently
;; the other success will cause the inner disjunction to fall back to the fallback
;; goal. The fallback goal will notify success; the fallback form needs to throw
;; that notify out to avoid making the outer fallback think this second outer branch
;; succeeded twice.
(defrel/generator (r x y outer inner)
  (fallback
   (later (== outer 'outer-fallback))
   (conde
     ((== x 1)
      (later (== outer 'outer-commit-1)))
     ((== x 2)
      (later (== outer 'outer-commit-2))
      (fallback
       (later (== inner 'inner-fallback))
       (conde
         ((== y 1)
          (later (== inner 'inner-commit-1)))
         ((== y 2)
          (later (== inner 'inner-commit-2)))))))))
(test
 (run 1 (outer inner)
   (staged
    (fresh (y)
      (r 2 y outer inner))))
 '((outer-commit-2 inner-fallback)))

;; However, a second success from the fallback goal *should* be notified to the
;; surrounding fallback form, leading it to fall back.
(defrel/generator (r2 x y outer inner)
  (fallback
   (later (== outer 'outer-fallback))
   (conde
     ((== x 1)
      (later (== outer 'outer-commit-1)))
     ((== x 2)
      (later (== outer 'outer-commit-2))
      (fallback
       (conde
         [(later (== inner 'inner-fallback-1))]
         [(later (== inner 'inner-fallback-2))])
       (conde
         ((== y 1)
          (later (== inner 'inner-commit-1)))
         ((== y 2)
          (later (== inner 'inner-commit-2)))))))))
(test
 (run 1 (outer inner)
   (staged
    (fresh (y)
      (r2 2 y outer inner))))
 '((outer-fallback _.0)))


;; Regression test: partial-apply needs to notify success to trigger fallback of
;; surrounding branches; initially it did not. Right now, it uses ss:promise-success
;; to notify before even running the partial's generator, and unique-result to require
;; that generator to succeed.  TODO: in the future I would like to make it okay for the
;; generator to fail.
(defrel/generator (p-g a b)
  (later (== a b)))

(defrel-partial (p [a] [b])
  #:generator p-g
  (== a b))

(test
 (run 1 (q)
   (staged
    (fallback
     (later (== q 3))
     (conde
       ((later (== q (partial-apply p 5))))
       ((later (== q 1)))))))
 '(3))

;; Regresssion test: similarly, later conde needs to notify. Currently it does this by
;; promising and requring success of everything within, but TODO: I'd like to change this
;; to only succeed if the conjunction generating all branches succeeds.
(test
 (run 1 (q)
   (staged
    (fallback
     (later (== q 4))
     (conde
       ((later (== q 1)))
       ((later
         (conde
           [(now (== q 2)) (== q 2)]
           [(now (== q 3)) (== q 3)])))))))
 '(4))


;; TODO: I think this is really a test of cross-stage persistence, if anything?
;;
;; Here the single branch succeeds at staging time so we generate the code from that branch,
;; but we discover a contradiction at run time. This results in run time failure.
(test
 (run 1 (q)
      (staged
       (fallback
        (later (== 1 q))
        (conde
          ((== 2 q) (later (== 3 q)))))))
 '())

;; TODO: another test of cross-stage persistence. 
(test
 (run 1 (q)
      (staged
       (fallback
        (later (== 1 q))
        (conde
          ((fresh (x)
             (== 2 x)
             (later (== q x))))))))
 '(2))
