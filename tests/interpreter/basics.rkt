#lang racket/base

(require "../../all.rkt" racket/list plot)

(provide plot-appendo-sizes)

(test
    (length
     (run 1 (q)
       (staged
        (evalo-staged `(lambda (x) x) q))))
  1)

(test
    (run 1 (q)
      (staged
       (evalo-staged `((lambda (x) x) 1) q)))
  '(1))

(test
    (length
     (run 1 (q)
       (staged
        (evalo-staged `(cons 1 2) q))))
  1)

(test
    (length
     (run 1 (q)
       (staged
        (evalo-staged `(lambda (f) (f 1)) q))))
  1)

(test
    (run 1 (q)
      (staged
       (evalo-staged
        `((lambda (f) (f 1))
          (lambda (x) x))
        q)))
  '(1))

(test
    (run 1 (q)
      (staged
       (evalo-staged
        `(letrec ((f (lambda (x) x)))
           (f 1))
        q)))
  '(1))

#;
(test
    (length
     (run 1 (q)
       (fresh (v)
         (evalo-staged
          q
          v))))
  1)

(test
    (run 1 (q)
      (staged
       (evalo-staged
        `(and #f #t)
        q)))
  '(#f))

(test
    (run 1 (q)
      (staged
       (evalo-staged
        `(and (and #t #t) (and #t #t))
        q)))
  '(#t))

(test
    (run 1 (q)
      (staged
       (fresh (x)
         (evalo-staged
          `(and ,x #t)
          #f))))
  '(_.0))

(test
    (length
     (run 2 (q)
       (staged
        (fresh (x y)
          (== (list x y) q)
          (evalo-staged
           `(and ,x #t)
           y)))))
  2)

#;(test
    (run 2 (q)
      (staged
       (evalo-staged
        `(and ,q #t)
        q)))
   '(#t #f))

(test
    ;; run 4 should generate a quine
    (run 3 (q)
      (staged
       (evalo-staged
        `(and #t ,q)
        q)))
  '((_.0 $$ (num _.0)) #t #f))

(test
    (run 3 (q)
      (staged
       (evalo-staged
        `(or #t . ,q)
        #t)))
  '(() (_.0 . _.1)))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (evalo-staged
          `(or #f . ,q)
          1)
         (later (== '(1) q)))))
  '((1)))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (evalo-staged
          `(or #f . ,q)
          1)
         (later (== '(#f 1) q)))))
  '((#f 1)))

(test
    (run 1 (q)
      (staged
       (evalo-staged
        `(letrec ((append
                   (lambda (xs ys)
                     (if (null? xs) ,q
                         (cons (car xs) (append (cdr xs) ys))))))
           (append '(1 2) '(3 4)))
        '(1 2 3 4))))
  '(ys))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (evalo-staged `(,q (list 1 2)) 1)
         (later (==  q 'car)))))
  '(car))

(test
    (run* (q)
      (staged
       (fresh ()
         (later (== q 'car))
         (evalo-staged `(,q (list 1 2)) 1))))
  '(car))

(test
    (length
     (run 1 (q)
       (staged
        (fresh (q1 q2)
          (later (== q `(,q1 ,q2)))
          (evalo-staged `(,q1 (list ,q2 2)) q2)))))
  1)

(test
    (length
     (run 1 (q1 q2)
       (staged
        (evalo-staged `(,q1 (list ,q2 2)) q2))))
  1)

(defrel (appendo xs ys zs)
  (conde
    ((== xs '()) (== ys zs))
    ((fresh (xa xd zd)
       (== (cons xa xd) xs)
       (== (cons xa zd) zs)
       (appendo xd ys zd)))))

(test
    (run* (xs ys)
      (appendo xs ys '(a b c)))
  '((() (a b c)) ((a) (b c)) ((a b) (c)) ((a b c) ())))

(defrel (ex e)
  (staged
   (evalo-staged `(cons ,e '()) '(5))))

(defrel (appendo-unstaged xs ys zs)
  (evalo-unstaged
   `(letrec ((append
              (lambda (xs ys)
                (if (null? xs)
                    ys
                    (cons (car xs)
                          (append (cdr xs) ys))))))
      (append ',xs ',ys))
   zs))

(defrel (appendo-staged xs ys zs)
  (staged
   (evalo-staged
    `(letrec ((append
               (lambda (xs ys)
                 (if (null? xs)
                     ys
                     (cons (car xs)
                           (append (cdr xs) ys))))))
       (append ',xs ',ys))
    zs)))

(define-syntax-rule (time-form body ...)
  (let-values ([(_ __ wall-clock-time ___)
                (time-apply
                 (lambda ()
                   body ...)
                 '())])
    wall-clock-time))

(define (appendo-size-timed size)
  (define lst (range size))
  (values
   (time-form (run* (p q) (appendo p q lst)))
   (time-form (run* (p q) (appendo-staged p q lst)))
   (time-form (run* (p q) (appendo-unstaged p q lst)))))

(define (plot-appendo-sizes [sizes (in-range 100 500 10)] [out-file #f])
  (define-values (handwritten staged unstaged)
    (for/lists (handwritten staged unstaged)
               ([size sizes])
      (printf "Plotting appendo sizes ~a~%" size)
      (define-values (handwritten staged unstaged) (appendo-size-timed size))
      (values (list size handwritten)
              (list size staged)
              (list size unstaged))))
  (plot (list (lines handwritten #:color 'green #:label "Handwritten")
              (lines staged #:color 'blue #:label "Staged")
              (lines unstaged #:color 'red #:label "Unstaged"))
        #:x-label "List Size"
        #:y-label "Time (ms)"
        #:title "Handwritten/Staged/Unstaged Runtime vs List Size"
        #:out-file out-file))

(test
    (run* (xs ys)
      (appendo-staged xs ys '(a b c)))
  '((() (a b c)) ((a) (b c)) ((a b) (c)) ((a b c) ())))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (later (== q '(lambda () (lambda (x) x))))
         (evalo-staged `((,q) 1) 1))))
      
  '((lambda () (lambda (x) x))))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (evalo-staged `((,q) 1) 1)
         (later (== q '(lambda () (lambda (x) x)))))))
      
    '((lambda () (lambda (x) x))))

(test
    (run 1 (q)
      (evalo-unstaged `(((lambda () car)) (cons 1 2)) 1))
      
  '(_.0))

(test
    (run 1 (q)
      (staged
       (evalo-staged `((lambda () car)) q)))
      
  '((struct prim . car)))

(test
    (run 1 (q)
      (staged
       (evalo-staged `(((lambda () car)) (cons 1 2)) 1)))
      
  '(_.0))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (== q '(lambda () car))
         (evalo-staged `((,q) (cons 1 2)) 1))))
      
  '((lambda () car)))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (later (== q '(lambda () car)))
         (evalo-staged `((,q) (cons 1 2)) 1))))
      
    '((lambda () car)))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (evalo-staged `((,q) (cons 1 2)) 1)
         (later (== q '(lambda () car))))))
      
  '((lambda () car)))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (symbolo q)
         (later (numbero q)))))
  '())

(defrel (bogus-appendo xs ys zs)
  (staged
   (evalo-staged
    `(letrec ((append
               (lambda (xs ys)
                 (if (null? xs)
                     ys
                     (cons (car xs)
                           (append (cdr xs) ys))))))
       append)
    zs)))

;; TODO: is this weird?
;;       it's not understood by evalo-unstaged
(test
    (length (run 1 (q)
              (bogus-appendo '(1 2) '(3 4) q)))
  1)

(test
    (run 1 (q)
      (staged
       (evalo-staged '(list 1 2 3) q)))
  '((1 2 3)))

;; Because this is a cross-stage test, and the staged and unstaged
;; are now separate, use the runtime interp generated from staged instead
;; of evalo-unstaged.
(test
    (length
     (run 1 (q p e)
       (staged
        (fresh ()
          (eval-expo `(x 1) `((x . (val . ,p))) q)
          (later (evalo-staged
                  `(letrec ((f ,e)) f) p))))))
  1)

;; With new fallback search this now specializes and our bug with losing substitution
;; extensions from capture-later means we lose the fact that the var x must be the
;; symbol x
(todo "remember lambda argument name"
    (run 1 (q x)
      (staged
       (fresh ()
         (symbolo x)
         (evalo-staged `((lambda (,x) x) 1) q))))
  '((1 x)))

(test
    (length
     (run 3 (q x y)
       (staged
        (fresh ()
          (symbolo x)
          (symbolo y)
          (=/= x 'lambda)
          (=/= y 'lambda)
          (evalo-staged `(((lambda (,x) (lambda (,y) z)) 1) 2) q)))))
  2)
;; NOTE: res has a u-lookupo call!

(test
    (run 1 (q)
      (staged
       (evalo-staged '(match '(hello) [`(hello ,x) 1]) q)))
  '())
  

(test
    (run 1 (q)
      (staged
       (evalo-staged '(match '(hello) [`(hello ,x) 1] [`(,x) 2]) q)))
  '(2))
  

;; regression test---this raised a problem when reflecting datums to quasiquotes
;; because fix-scope special cased `quote` and didn't understand quasiquote.
(test
 (run 1 (q) (staged (later (evalo-unstaged `(quote ,q) 5))))
 '(5))
