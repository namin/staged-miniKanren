#lang racket/base

(require "../generator-lang2.rkt"
         "../test-check.rkt")

(defrel (unify-5 x)
  (== x 5))

(test
 (run 1 (q) (unify-5 5))
 '(_.0))

(defrel (bar a)
  (staged
   (later
    (== a 1))))

(test
 (run 1 (q)
   (bar q))
 '(1))

(test
 (run 1 (q)
   (staged
    (later (unify-5 q))))
 '(5))


(test
 (run 1 (q)
   (staged
    (fresh (x)
      (== x 1)
      (conde
       [(== x 1)
        (later (== q 1))]
       [(== x 2)
        (later (== q 2))]))))
 '(1))

(test
 (run 2 (q)
   (staged
    (fresh (x y)
      (later
       (fresh ()
         (== x q)
         (conde
          [(now (symbolo x)) (== y 1)]
          [(now (numbero x)) (== y 1)]))))))
 '((_.0 $$ (sym _.0)) (_.0 $$ (num _.0))))

(test
    (run 1 (q)
      (staged
       (fresh (x y)
         (later (== q (cons x y))))))
  '((_.0 . _.1)))

;; TODO: this generates code for the symbolo and numbero even though they aren't for a variable that's relevant at runtime
(test
 (run 2 (q)
   (staged
    (fresh (x y)
      (later
       (fresh ()
         (conde
          [(now (symbolo x)) (== y 1)]
          [(now (numbero x)) (== y 1)]))))))
 '(_.0 _.0))

(test
 (run 3 (q)
   (staged
    (fresh (x y)
      ;;(== q (cons x y))
      (later
       (fresh ()
         (conde
           [(now (symbolo x))
            (conde
              [(symbolo x)
               (== y 1)]
              [(numbero x)
               (== y 2)])]
          [(now (numbero x)) (== y 1)]))))))
 '(_.0 _.0))


(defrel-partial (test-rel [y z] [x res])
  #:generator test-rel-staged
  (fresh (yz)
    (== yz `(,y ,z))
    (== res (cons x yz))))

(defrel/generator (test-rel-staged rep y z x res)
  (fresh (yz)
    (== yz `(,y ,z))
    (later (== res (cons x yz)))))

;; partial application, all at runtime
(test
 (run 1 (q)
   (fresh (c r1 r2)
     (== c (partial-apply test-rel 2 3))
     (apply-partial c test-rel 1 r1)
     (apply-partial c test-rel 4 r2)
     (== q `(,r1 ,r2))))
'(((1 2 3) (4 2 3))))

;; partial application, all later (this is like fully-staged interpretation of closures)
(test
 (run 1 (q)
   (staged
     (fresh (c r1 r2)
       (later (== c (partial-apply test-rel 2 3)))
       (later (apply-partial c test-rel 1 r1))
       (later (apply-partial c test-rel 4 r2))
       (later (== q `(,r1 ,r2))))))
'(((1 2 3) (4 2 3))))

;; partial application, staged closure, call in runtime code
(defrel (like-callo c arg res)
  (apply-partial c test-rel arg res))
(test
 (run 1 (q)
   (staged
     (fresh (c r1 r2)
       (later (== c (partial-apply test-rel 2 3)))
       (later (like-callo c 1 r1))
       (later (like-callo c 4 r2))
       (later (== q `(,r1 ,r2))))))
 '(((1 2 3) (4 2 3))))

(defrel/generator (gen-unify-5 x)
  (later
   (== x 5)))

(test
 (run 1 (q)
   (staged
    (gen-unify-5 q)))
 '(5))


;; TODO: should be okay if there are no `later`s in a staged
(todo "no later staged should be OK"
 (run 1 (q)
   (staged
    (== q 1)))
 '(1))

;; TODO: this fails for two reasons.
;; - conde: bad syntax in: (conde () ())
;; - we don't capture substitution extensions in conde
(todo "now in later conde bug"
 (run 2 (q)
   (staged
    (fresh (x y)
      (== q x)
      (later
       (conde
        [(now (== x 1))]
        [(now (== x 2))])))))
 '(1 2))

(todo "now in later conde bug"
 (run 2 (q)
   (staged
    (fresh (x y)
      (== q x)
      (later
       (conde
        [(now (== x 1)) (== y 1)]
        [(now (== x 2)) (== y 1)])))))
 '(1 2))

;; TODO: this one is even harder! There's no way to tell at the end of capture-later
;; that the value of x will end up being relevant to the later stage.
;;
;; I think we have to reflect all store / substitution extensions and then do dead
;; code elimination on the final program.
(todo "now in later conde bug"
 (run 2 (q)
   (staged
    (fresh (x y)
      (later
       (conde
        [(now (== x 1)) (== 1 1)]
        [(now (== x 2)) (== 1 1)]))
      (== q x))))
 '(1 2))
