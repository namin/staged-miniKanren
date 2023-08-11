#lang racket/base

(require "../main.rkt"
         "../test-check.rkt")

(defrel (eval-ambo e v)
  (conde
    ((numbero e)
     (== e v))
    ((fresh (e1 e2 v1 v2)
       (== e `(cons ,e1 ,e2))
       (== v (cons v1 v2))
       (eval-ambo e1 v1)
       (eval-ambo e2 v2)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (conde
         ((eval-ambo e1 v))
         ((eval-ambo e2 v)))))))

(test
  (run* (v) (eval-ambo '(amb 1 2) v))
  '(1 2))

(defrel/generator (gen-eval-ambo e v)
  (conde
    ((numbero e)
     (later (== e v)))
    ((fresh (e1 e2 v1 v2)
       (== e `(cons ,e1 ,e2))
       (later (== v (cons v1 v2)))
       (gen-eval-ambo e1 v1)
       (gen-eval-ambo e2 v2)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (gather
        (conde
          ((gen-eval-ambo e1 v))
          ((gen-eval-ambo e2 v))))))))

(test
  (run* (v) (staged (gen-eval-ambo '(amb 1 2) v)))
  '(1 2))

;; non-deterministic
;; (run 2 (e v) (staged (gen-eval-ambo `(cons (amb 1 2) ,e) v)))

(defrel/multistage/fallback (ms-eval-ambo e v)
  (conde
    ((numbero e)
     (later (== e v)))
    ((fresh (e1 e2 v1 v2)
       (== e `(cons ,e1 ,e2))
       (later (== v (cons v1 v2)))
       (ms-eval-ambo e1 v1)
       (ms-eval-ambo e2 v2)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (gather
        (conde
          ((ms-eval-ambo e1 v))
          ((ms-eval-ambo e2 v))))))))

(test
  (run* (v) (staged (ms-eval-ambo '(amb 1 2) v)))
  '(1 2))

(run 4 (e v) (staged (ms-eval-ambo `(cons (amb 1 2) ,e) v)))

