#lang racket/base

(require "../../private/internals.rkt"
         "../../test-check.rkt")

(test
 (length (ss:simple-run (ss:atomic (== 1 1))))
 1)

(test
 (length (ss:simple-run (ss:atomic (== 1 2))))
 0)

(test
 (length (ss:simple-run (ss:disj2 (ss:atomic (== 1 1)) (ss:atomic (== 1 2)))))
 1)

(test
 (length (ss:simple-run
          (ss:fresh (x y)
                    (ss:disj (ss:atomic (== 1 2)) (ss:atomic (== x 2)) (ss:atomic (== x 1)))
                    (ss:atomic (== x y))
                    (ss:atomic (== y 2)))))
 1)

(test
 (length
  (ss:simple-run
   (ss:fresh (x y)
             (ss:disj (ss:atomic (== 1 2))
                      (ss:atomic (== x 2))
                      (ss:atomic (== x 1)))
             (ss:atomic (== x y))
             (ss:atomic (== y 2)))))
 1)

(test
 (length
  (ss:simple-run
   (ss:fresh (x y z)
             (ss:maybe-fallback
              (ss:atomic (l== x 3))
              (ss:disj (ss:atomic (l== x 1))
                       (ss:atomic (l== x 2))))
             (ss:maybe-fallback
              (ss:atomic (l== y 3))
              (ss:disj (ss:atomic (l== y x))
                       (ss:atomic (l== y z)))))))
 1)

(ss:generate-staged (x y)
 (ss:fresh (z)
           (ss:maybe-fallback
            (ss:atomic (l== x 3))
            (ss:disj (ss:atomic (l== x 1))
                     (ss:atomic (l== x 2))))
           (ss:maybe-fallback
            (ss:atomic (l== y 3))
            (ss:disj (ss:atomic (l== y x))
                     (ss:atomic (l== y z))))))

(generated-code)



(ss:generate-staged (x y)
 (ss:fresh (z)
           (ss:maybe-fallback
            (ss:atomic (l== x 3))
            (ss:disj (ss:atomic (l== x 1))
                     (ss:atomic (l== x 2))))
           (ss:ldisj
            (ss:atomic (l== y x))
            (ss:atomic (l== y z)))))

(generated-code)