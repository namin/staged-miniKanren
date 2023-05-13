#lang racket/base

(require "all.rkt")

(run 3 (q)
     (evalo-unstaged
       `(or #t . ,q)
       #t))

(run 3 (e)
     (u-oro `(#t . ,e) initial-env #t))

(run 3 (e)
     (staged (oro `(#t) initial-env #t)))

#;(run 3 (e)
     (fresh (e1 e2)
            (staged (oro `(#t ,e1 . ,e2) initial-env #t))))

(run 1 (e)
     (staged (oro `() initial-env #t)))

(generated-code)

;; here it figures we need to fall back, good!
(run 1 (e)
     (staged (oro e initial-env #t)))

(generated-code)

;; what's the recursive case here going to be?
;; Ah, it's always going to be instantiated to (e2 . e-rest).
;; The empty case is never going to match it.
;; Seems like the single case should match.
(run 1 (e1 e2)
  (staged (oro `(,e1 . ,e2) initial-env #t)))

;; (or anything) is a base case.
;; (or e1 e2 . e-rest) is the recursive case.
;;
;; Both unifications match when the length of the or subexpression list
;; is not instantiated.
;;

;; So I would *want* to fall back here. Why don't we?
;;   well, the base case option should succeed.
;;   what about the recursive case option?
;;      call for the head expression will succeed
;;      then we get to the later conde
;;        first case is fine
;;        second case wants to generate for the recursion

#;(run 3 (q)
     (staged
      (evalo-staged
       `(or #t . ,q)
       #t)))