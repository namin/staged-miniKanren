#lang racket/base

(require "../../main.rkt"
         "../../test-check.rkt")

(defrel-partial (test-rel [y z] [x res])
  #:generator test-rel-staged
  (fresh (yz)
    (== y z)
    (== yz `(,y ,z))
    (== res (cons x yz))))

(defrel/generator (test-rel-staged rep y z x res)
  (fresh (yz)
    (== y z)
    (== yz `(,y ,z))
    (later (== res (cons x yz)))))

;; All at runtime, like unstaged interpretation of closures.
;;
;; Note that the different applications of the same partial
;; have different values for the later-stage arguments.
(test
 (run 1 (q)
   (fresh (c r1 r2)
     (== c (partial-apply test-rel 2 2))
     (apply-partial c test-rel 1 r1)
     (apply-partial c test-rel 4 r2)
     (== q `(,r1 ,r2))))
 '(((1 2 2) (4 2 2))))

;; All later, like fully-staged interpretation of closures
(test
 (run 1 (q)
   (staged
    (fresh (c r1 r2)
      (later (== c (partial-apply test-rel 2 2)))
      (later (apply-partial c test-rel 1 r1))
      (later (apply-partial c test-rel 4 r2))
      (later (== q `(,r1 ,r2))))))
 '(((1 2 2) (4 2 2))))

;; Mixed stage, like with a staged closure called in runtime code
(defrel (like-callo c arg res)
  (apply-partial c test-rel arg res))
(test
 (run 1 (q)
   (staged
    (fresh (c r1 r2)
      (later (== c (partial-apply test-rel 2 2)))
      (later (like-callo c 1 r1))
      (later (like-callo c 4 r2))
      (later (== q `(,r1 ,r2))))))
 '(((1 2 2) (4 2 2))))

;; A later partial-apply fails if the generator fails.
(test
 (run 1 (q)
   (staged
    (conde
      [(fresh (c)
         (later (== c (partial-apply test-rel 2 3)))
         (later (== q 'branch-1)))]
      [(later (== q 'branch-2))])))
 '(branch-2))
      