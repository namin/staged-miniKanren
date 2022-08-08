(test
    (run-staged 1 (q)
      (l== q 1))
  '(1))

#|
;; not deterministic
(run-staged 2 (q)
  (conde
    ((l== q 1))
    ((l== q 2))))
|#

(test
    (run-staged 2 (q)
      (later `(conde
               ((== ,q 1))
               ((== ,q 2)))))
  '(1 2))

(test
    (length
     (run-staged 1 (q)
       (evalo-staged `(lambda (x) x) q)))
  1)

(test
    (run-staged 1 (q)
      (evalo-staged `((lambda (x) x) 1) q))
  '(1))

(test
    (run-staged 1 (q)
      (evalo-staged
       `((lambda (f) (f 1))
         (lambda (x) x))
       q))
  '(1))

(test
    (run-staged 1 (q)
      (evalo-staged
       `(letrec ((f (lambda (x) x)))
          (f 1))
       q))
  '(1))

(test
    (run-staged 1 (q)
      (evalo-staged
       `(letrec ((append
                  (lambda (xs ys)
                    (if (null? xs) ,q
                        (cons (car xs) (append (cdr xs) ys))))))
          (append '(1 2) '(3 4)))
       '(1 2 3 4)))
  '(ys))

(test
    (run-staged 1 (q)
      (evalo-staged `(,q (list 1 2)) 1)
      (l== q 'car))
  '(car))

(test
    (run-staged* (q)
      (l== q 'car)
      (evalo-staged `(,q (list 1 2)) 1))
  '(car))

(test
    (length
     (run-staged 1 (q)
       (fresh (q1 q2)
         (l== q `(,q1 ,q2))
         (evalo-staged `(,q1 (list ,q2 2)) q2))))
  1)

(test
    (length
     (run-staged 1 (q1 q2)
       (evalo-staged `(,q1 (list ,q2 2)) q2)))
  1)

(define-relation (appendo xs ys zs)
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

(define-staged-relation (ex e)
  (evalo-staged `(cons ,e '()) '(5)))

(define-staged-relation (appendo-staged xs ys zs)
  (evalo-staged
   `(letrec ((append
              (lambda (xs ys)
                (if (null? xs)
                    ys
                    (cons (car xs)
                          (append (cdr xs) ys))))))
      (append ',xs ',ys))
   zs))

(test
    (run* (xs ys)
      (appendo-staged xs ys '(a b c)))
  '((() (a b c)) ((a) (b c)) ((a b) (c)) ((a b c) ())))

(test
    (run-staged 1 (q)
      (l== q '(lambda () (lambda (x) x)))
      (evalo-staged `((,q) 1) 1)
      )
  '((lambda () (lambda (x) x))))

(test
    (run-staged 1 (q)
      (evalo-staged `((,q) 1) 1)
      (l== q '(lambda () (lambda (x) x)))
      )
    '((lambda () (lambda (x) x))))

(test
    (run 1 (q)
      (evalo-unstaged `(((lambda () car)) (cons 1 2)) 1)
      )
  '(_.0))

(test
    (run-staged 1 (q)
      (evalo-staged `((lambda () car)) q)
      )
  '((prim . car)))

(test
    (run-staged 1 (q)
      (evalo-staged `(((lambda () car)) (cons 1 2)) 1)
      )
  '(_.0))

res

(test
    (run-staged 1 (q)
      (== q '(lambda () car))
      (evalo-staged `((,q) (cons 1 2)) 1)
      )
  '((lambda () car)))

(test
    (run-staged 1 (q)
      (l== q '(lambda () car))
      (evalo-staged `((,q) (cons 1 2)) 1)
      )
    '((lambda () car)))

(test
    (run-staged 1 (q)
      (evalo-staged `((,q) (cons 1 2)) 1)
      (l== q '(lambda () car))
      )
  '((lambda () car)))

(test
    (run-staged 1 (q)
      (symbolo q)
      (later `(numbero ,q)))
  '())

(define-staged-relation (bogus-appendo xs ys zs)
  (evalo-staged
   `(letrec ((append
              (lambda (xs ys)
                (if (null? xs)
                    ys
                    (cons (car xs)
                          (append (cdr xs) ys))))))
      append)
   zs))

;; TODO: is this weird?
;;       it's not understood by evalo-unstaged
(test
    (length (run* (q)
              (bogus-appendo '(1 2) '(3 4) q)))
  1)

(test
    (run-staged 1 (q)
      (evalo-staged '(list 1 2 3) q))
  '((1 2 3)))


(test
    (length
     (run-staged 1 (q p e)
       (eval-expo `(x 1) `((x . (val . ,p))) q)
       (later `(evalo-unstaged
                ,(expand `(letrec ((f ,(unexpand e))) f)) ,p))))
  1)

(test
    (run-staged 1 (q)
      (evalo-staged '(match '(hello) [`(hello ,x) 1]) q))
  '()
  )

(test
    (run-staged 1 (q)
      (evalo-staged '(match '(hello) [`(hello ,x) 1] [`(,x) 2]) q))
  '(2)
  )
