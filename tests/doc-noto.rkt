#lang racket

(require "../all.rkt")

(defrel/staged (noto p q)
  (conde
   [(== p #t) (== q #f)]
   [(== p #f) (== q #t)]))

(test
 (run* (p q)
   (fresh (x)
     (noto p x)
     (noto #f q)))
 '((#t #t) (#f #t)))

;; This fails at compile-time, as staging is nondeterministic
#; 
(run 1 (p q)
  (staged (noto p q)))

(defrel/staged (noto/gather p q)
  (gather
   (conde
    [(== p #t) (== q #f)]
    [(== p #f) (== q #t)])))

(defrel/staged (noto/fallback p q)
  (fallback
   (conde
    [(== p #t) (== q #f)]
    [(== p #f) (== q #t)])))

(test
 (run* (p q)
   (staged
    (fresh (x)
      (noto/gather #f q)
      (noto/gather p x))))
 '((#t #t) (#f #t)))

(generated-code)

(test
 (run* (p q)
   (staged
    (fresh (x)
      (noto/fallback #f q)
      (noto/fallback p x))))
 '((#t #t) (#f #t)))

(generated-code)
