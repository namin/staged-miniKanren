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


;; The discovery of two possible answers anywhere under a fallback triggers
;; use of the fallback goal even when there are other branches that expand indefinitely.
;;
;; In this example the success of branch-1 and branch-2
;; triggers the outer fallback, even though the `nevero` never terminates.
;;
;; Note that this design means that the implementation of fallback cannot simply run*
;; all the branches to determine if there is nondeterminism! With that strategy, the
;; inner fallback form would fail to terminate. Instead, the computation terminates when
;; the outer fallback finds out about the success of branch-2 and triggers fallback-1.
(defrel/generator (nevero)
  (nevero))
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
;;
;; To see why the above behavior matters in practice, consider `membero`:
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
;; And in particular, the situation where the list is unknown:
(test
 (run 1 (q)
   (staged
    (fresh (l)
      (membero 'x l q))))
 '(fallback))
;; This is analogous to situations in the staged interpreter where the expression
;; is unknown.
;;
;; We want fallback to happen in this example. To achieve that, evaluation has
;; to determine that there are multiple answers, in particular l = (x . _.0) and
;; l = (_.0 x . _.1) w/ (=/= x _.0).
;;
;; But the outer fallback must not run* its second branch, because the recursive
;; case can expand to arbitrary depth! The outer fallback must come to know about
;; the l = (_.0 x . _.1) w/ (=/= 'x _.0) answer from its recursive case without
;; forcing evaluation of the rest of the recursive case.
;;
;; This is the first of two constraints that make the implementation tricky.


;; In general it is not enough to simply know that an inner fallback has an answer.
;; Determining whether an outer fallback should trigger may require running further
;; goals in a conjunction with the result of an inner fallback.
;;
;; Here, we must run the (== x 2) goal outside of the inner fallback in order to
;; discover that the answer from the inner fallback actually leads the outer fallback
;; to be nondeterministic:
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
;; This means that the first solution of the inner fallback must be tried with the
;; success-k of the inner fallback before we have committed to the decision of whether
;; to fall back or not.
;;
;; We must interleave evaluation of (a) the further conjuncts based on the inner fallback's
;; with (b) attempting to find additional solutions to the inner fallback's body:
;;   - (a) may never succeed, in which case the only way to succeed overall is to discover
;;    more solutions from (b) to trigger the inner fallback
;;   - (b) may never succeed, in which case (a) is the only way to succeed overall.
;;
;; But in the case that (b) ever succeeds, our semantics demands that we return the fallback
;; result and not the result of (a)! So when (b) succeeds, we have to claw back and cancel
;; the work on (a).
;;
;; This is the second constraint that makes the implementation challenging.


;; To simultaneously solve the two constraints, the implementation must *notify* surrounding
;; fallbacks that a solution has been reached while still retaining control over the solution
;; in order to be able to replace it with the fallback if another solution is reached.
;;
;; Outer fallback forms count the notifications, and fallback if they receive more than one
;; such notification.


;; The remaining tests relate to the way these notifications propagate through other
;; kinds of goals.

;; Success of the first goal in a conjunct must not notify uccess of the whole conjunction.
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
;;
;; See the use of `ss:tag-notify` and `ss:filter-notify` in `ss:conj2` in the implementation.

;; notify-success while evaluating a conjunct outside of and after a fallback (that is,
;; in the success continuation given to the fallback) should not be counted by the fallback
;; as indicating nondeterminism.
(test
 (run 1 (x)
      (staged
       (fresh ()
         (fallback
          (later (== x 'fallback))
          (later (== x 'only-answer)))
         (== 1 1))))
 '(only-answer))
;; However, the notification must propagate out to trigger outer fallbacks.
;;
;; See success-k^ and the (set-member? tags ignore-tag) checks inside ss:maybe-fallback.


;; Success of the fallback goal should not cause an outer
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
;; See ss:drop-one-notify and its use in ss:maybe-fallback in the implementation.



;; partial-apply needs to notify success to trigger fallback of surrounding branches.
;; Right now, it uses ss:promise-success
;; to notify before even running the partial's generator, and unique-result to require
;; that generator to succeed.  TODO: in the future I would like to make it okay for the
;; generator to fail.
(defrel/generator (p-g rep a b)
  (later (== a b)))

(defrel-partial (p [a] [b])
  #:generator p-g
  (== a b))

(test
 (run 1 (q)
   (staged
    (fallback
     (later (== q 'fallback))
     (conde
       ((fresh (rep)
          (later (== rep (partial-apply p 5)))
          (== q 'branch-1)))
       ((later (== q 'branch-2)))))))
 '(fallback))

(test
 (run 1 (q)
   (staged
    (fallback
     (later (== q 'fallback))
     (fresh (rep)
       (later (== rep (partial-apply p 'partial-value)))
       (later (apply-partial rep p q))))))
 '(partial-value))

;; Similarly, later conde needs to notify. Currently it does this by
;; promising and requring success of everything within, but TODO: I'd like to change this
;; to only succeed if the conjunction generating all branches succeeds.
(test
 (run 1 (q)
   (staged
    (fallback
     (later (== q 4))
     (conde
       ((later (== q 1)))
       ((gather
         (conde
           [(== q 2) (later (== q 2))]
           [(== q 3) (later (== q 3))])))))))
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
