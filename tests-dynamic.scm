(load "staged-mk.scm")

(load "staged-utils.scm")

;; blurring distinction
;; between logic variable and code

;; fully now
(run* (q) (== 1 q))

;; explicitly deferred
(run* (q) (l== 1 q))

;; implicitly deferred
(run* (q) (dynamic q) (== 1 q))
(run* (q) (dynamic q) (== (list 1) (list q)))

;; what to do about constraints?
(run* (q) (dynamic q) (=/= 1 q))

;; staging with vanilla interp!
(load "full-interp.scm")
(run* (q) (dynamic q) (eval-expo 1 '() q))

(run 1 (q)
  (fresh (arg res) (== q (list arg res))
    (eval-expo 'x `((x . (val . ,arg))) res)))

(run 1 (q)
  (fresh (arg res)
    (dynamic arg res)
    (== q (list arg res))
    (eval-expo 'x `((x . (val . ,arg))) res)))

;; The examples with if show that each branch is grounded
;; into an answer.

(run 1 (q)
  (fresh (arg res env)
    (dynamic arg res)
    (== q (list arg res))
    (ext-env*o '(x) `(,arg) initial-env env)
    (eval-expo '(if (null? '()) 1 2) env res)))

(run* (q)
  (fresh (arg res env)
    (== q (list arg res))
    (ext-env*o '(x) `(,arg) initial-env env)
    (eval-expo '(if (null? x) 1 2) env res)))

(run* (q)
  (fresh (arg res env)
    (dynamic arg res)
    (== q (list arg res))
    (ext-env*o '(x) `(,arg) initial-env env)
    (eval-expo '(if (null? x) 1 2) env res)))
;; =>
'(((_.0 _.1) !! ((== '() _.0) (== _.1 '1)))
  (((_.0 _.1) !! ((== _.2 _.0) (== _.1 '2)))
   (=/= ((_.2 ())))))

;; The if-grounding means that for a recursive relation,
;; we ground on the recursive structure...
;; This is not symbolic enough, it's suggestive
;; of limitations SMT has with recursion.
(run 2 (q)
  (fresh (xs ys res env)
    (dynamic xs ys)
    (== q (list xs ys res))
    (ext-env*o '(xs ys) (list xs ys) initial-env env)
    (eval-expo `(letrec
                    ((append (lambda (xs ys)
                               (if (null? xs) ys
                                   (cons (car xs)
                                         (append (cdr xs) ys))))))
                  (append xs ys))
               env
               res)))

;; next step
;; what is an alternative to semantics to if-grounding for dynamic variables?

;; here is one alternative:
(load "dynamic-interp.scm")
(define appendo
  (eval
   (gen 'append '(xs ys)
        '(if (null? xs) ys
             (cons (car xs)
                   (append (cdr xs) ys))))))

(run* (q) (appendo '(a) '(b) q))
(run* (q) (appendo q '(b) '(a b)))
(run* (q) (fresh (x y) (== q (list x y)) (appendo x y '(a b c d e))))
