#lang racket/base

(require "../../main.rkt"
         "../../test-check.rkt")

;; When there's only one branch, we should get that answer rather than fall back.
(test
 (run 1 (q)
      (staged
       (fallback
        (later (== q 1))
        (conde
          ((== q 2) (later (== q 2)))))))
 '(2))

;; When there are multiple successful branches we should discard their results and fall back.
(test
 (run 1 (q)
      (staged
       (fallback
        (later (== 1 q))
        (conde
          ((== q 2) (later (== q 2)))
          ((== q 3) (later (== q 3)))))))
 '(1))

;; When only one branch can succeed because of previous constraints, we should get that answer
;; rather than fall back.
(test
 (run 1 (q)
      (staged
       (fresh ()
         (== q 3)
         (fallback
          (later (== q 1))
          (conde
            ((== q 2) (later (== q 2)))
            ((== q 3) (later (== q 3))))))))
 '(3))

;; Results can also be filtered after the branch but before returning from the fallback.
(test
 (run 1 (q)
   (staged
    (fallback
     (later (== q 1))
     (fresh ()
       (conde
         ((== q 2) (later (== q 2)))
         ((== q 3) (later (== q 3))))
       (== q 3)))))
 '(3))

;; After exiting the fallback form, it is too late for new information to filter branches
;; and avoid fallback.
;;
;; TODO: is this a good thing? One could also imagine following a branch all the way through
;; to the halt continuation before committing to the fallback... But I think this would lead
;; to diverging searches at staging time when you wanted to defer the divergence to runtime.
(test
 (run 1 (q)
      (staged
       (fresh (x)
         (fallback
          (later (== q 1))
          (conde
            ((== x 2) (later (== q 2)))
            ((== x 3) (later (== q 3)))))
         (== x 3))))
 '(1))


;; Recognizing the need to fall back here is made tricky by the recursion nesting
;; fallback forms. For some fallback to fall back, it has to know that both its base
;; and recursive cases can succeed. But its recursive case contains a nested fallback
;; form with the same challenge! We can only successfully fall back if the inner fallback
;; reports out that it has a base-case branch that can succeed, thus allowing the outer
;; fallback form to know it has at least two solutions.
(defrel/generator (membero x l log)
  (fallback
   (later (== log 'fallback))
   (fresh (first rest log-rest)
     (== `(,first . ,rest) l)
     (conde
       [(== x first)
        (== log 'commit-base-case)]
       [(=/= x first)
        (== log `(commit-recursive-case . ,log-rest))
        (membero x rest log-rest)]))))
(test
 (run 1 (q)
   (staged
    (fresh (l)
      (membero 'x `(y . ,l) q))))
 '((commit-recursive-case . fallback)))

;; Regression test: success of the first goal in a conjunct must not notify
;; success of the whole conjunction.
;;
;; In this example the first unification in the first branch (== q 2) will
;; succeed, but the overall branch will fail. We don't want to notify-success
;; for this branch and thus end up falling back.
(test
 (run 1 (q)
      (staged
       (fresh (x)
         (== x 3)
         (fallback
          (later (== 1 q))
          (conde
            ((== q 2) (== x 2) (later (== q 2)))
            ((== q 3) (== x 3) (later (== q 3))))))))
 '(3))

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
