#lang racket/base

(require racket/pretty
         "../main.rkt"
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

(test
  (run* (v) (eval-ambo '(amb 1 (amb 2 3)) v))
  '(1 2 3))

(test
  (run* (v) (eval-ambo '(cons (amb 1 2) (amb 3 4)) v))
  '((1 . 3) (1 . 4) (2 . 3) (2 . 4)))

(defrel/staged (gen-eval-ambo e v)
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
(pretty-print (generated-code))

;; non-deterministic
;; (run 2 (e v) (staged (gen-eval-ambo `(cons (amb 1 2) ,e) v)))

;; Will's challenge
;; this doesn't terminate because the recursion can keep going forever.
(defrel/staged (gen-eval-ambo-v e v)
  (conde
    ((numbero v)
     (gather
      (conde
        ((later (== e v)))
        ((fresh (e1 e2)
           (later (== e `(amb ,e1 ,e2)))
           (gather
            (conde
              ((gen-eval-ambo-v e1 v))
              ((gen-eval-ambo-v e2 v)))))))))
    ((fresh (v1 v2)
       (== (cons v1 v2) v)))))

(defrel/staged (ms-eval-ambo e v)
  (fallback
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
           ((ms-eval-ambo e2 v)))))))))

(test
  (run* (v) (staged (ms-eval-ambo '(amb 1 2) v)))
  '(1 2))

(run 4 (e v) (staged (ms-eval-ambo `(cons (amb 1 2) ,e) v)))
(pretty-print (generated-code))

(defrel (lookupo x env v)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
     [(== x y) (== v b)]
     [(=/= x y) (lookupo x rest v)])))

(defrel-partial (apply-lambda-ambo rep [x e env] [arg v])
  (eval-lambda-ambo e (cons (cons x arg) env) v))

(defrel (eval-lambda-ambo e env v)
  (conde
    ((numbero e)
     (== e v))
    ((fresh (e1 e2 v1 v2)
       (== e `(cons ,e1 ,e2))
       (== v (cons v1 v2))
       (eval-lambda-ambo e1 env v1)
       (eval-lambda-ambo e2 env v2)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (conde
         ((eval-lambda-ambo e1 env v))
         ((eval-lambda-ambo e2 env v)))))
    ((symbolo e)
     (lookupo e env v))
    ((fresh (x e0)
       (== e `(lambda (,x) ,e0))
       (== v (partial-apply apply-lambda-ambo x e0 env))))
    ((fresh (e1 e2 v1 v2)
       (== e `(,e1 ,e2))
       (eval-lambda-ambo e1 env v1)
       (eval-lambda-ambo e2 env v2)
       (finish-apply v1 apply-lambda-ambo v2 v)))))

(test
  (run* (v) (eval-lambda-ambo '(amb 1 2) '() v))
  '(1 2))

(test
  (run* (v) (eval-lambda-ambo '((lambda (x) (amb x 3)) (amb 1 2)) '() v))
  '(3 3 1 2))

(defrel/staged (ms-lookupo x env v)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (fallback
     (conde
       [(== x y) (== v b)]
       [(=/= x y) (ms-lookupo x rest v)]))))

(defrel-partial/staged (ms-apply-lambda-ambo rep [x e env] [arg v])
  (ms-eval-lambda-ambo e (cons (cons x arg) env) v))

(defrel/staged (ms-eval-lambda-ambo e env v)
  (fallback
   (conde
     ((numbero e)
      (later (== e v)))
     ((fresh (e1 e2 v1 v2)
        (== e `(cons ,e1 ,e2))
        (later (== v (cons v1 v2)))
        (ms-eval-lambda-ambo e1 env v1)
        (ms-eval-lambda-ambo e2 env v2)))
     ((fresh (e1 e2)
        (== e `(amb ,e1 ,e2))
        (gather
         (conde
           ((ms-eval-lambda-ambo e1 env v))
           ((ms-eval-lambda-ambo e2 env v))))))
     ((fresh (env-v)
        (symbolo e)
        ;; We want to make the unifications with v later-stage.
        ;; We put it before the lookupo to make sure the runtime version knows the value.
        (later (== v env-v))
        (ms-lookupo e env env-v)))
     ((fresh (x e0)
        (== e `(lambda (,x) ,e0))
        (== v (specialize-partial-apply ms-apply-lambda-ambo x e0 env))))
     ((fresh (e1 e2 v1 v2)
        (== e `(,e1 ,e2))
        (ms-eval-lambda-ambo e1 env v1)
        (ms-eval-lambda-ambo e2 env v2)
        (later (finish-apply v1 ms-apply-lambda-ambo v2 v)))))))

(test
  (run* (v) (staged (ms-eval-lambda-ambo '(amb 1 2) '()  v)))
  '(1 2))

(run 4 (e v) (staged (ms-eval-lambda-ambo `(cons (amb 1 2) ,e) '()  v)))

(test
  (run* (v) (staged (ms-eval-lambda-ambo '((lambda (x) (amb x 3)) (amb 1 2)) '() v)))
  '(3 3 1 2))

#;
(defrel (condg-eval-ambo e v)
  (condg
   ([] [(numbero e) (== e v)] [])
   ([e1 e2 v1 v2]
    [(== e `(cons ,e1 ,e2))
     (== v (cons v1 v2))]
    [(condg-eval-ambo e1 v1)
     (condg-eval-ambo e2 v2)])
   ([e1 e2]
    [(== e `(amb ,e1 ,e2))]
    [(gather
      (conde
        ((condg-eval-ambo e1 v))
        ((condg-eval-ambo e2 v))))])))
