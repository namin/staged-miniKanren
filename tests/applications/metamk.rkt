#lang racket/base

(require racket/pretty
         "../../all.rkt")

;; simplified https://github.com/namin/metamk/blob/master/cycler-tests.scm

(defrel/staged/fallback (edgeo x y)
  (conde
    ((== x 'a) (== y 'b))
    ((== x 'b) (== y 'c))
    ((== x 'c) (== y 'a))))

(defrel/staged/fallback (clause head tail)
  (fresh (x y)
    (== head `(patho ,x ,y))
    (conde
      ((edgeo x y) (== tail '()))
      ((fresh (z)
         (edgeo x z)
         (== tail `((patho ,z ,y))))))))

(defrel/staged/fallback (solve* goals trace-in trace-out)
  (conde
    ((== goals '())
     (== trace-in trace-out))
    ((fresh (first-goal other-goals first-body trace-out-body)
       (== (cons first-goal other-goals) goals)
       (absento first-goal trace-in)
       (clause first-goal first-body)
       (solve* first-body (cons first-goal trace-in) trace-out-body)
       (solve* other-goals trace-out-body trace-out)))))

(defrel/staged/fallback (cycler* goals trace)
  (solve* goals '()  trace))

(defrel/staged/fallback (cycler goal trace)
  (cycler* (list goal) trace))

(run* (q)
  (fresh (head tail)
    (== q `(to prove ,head prove ,tail))
    (clause head tail)))

(run* (x y t)
  (cycler `(patho ,x ,y) t))

(run 1 (t)
  (staged
   (cycler `(patho c c) t)))

(pretty-print (generated-code))
