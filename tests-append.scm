(load "staged-load.scm")

(define (appendo xs ys zs)
  (conde
    ((== xs '()) (== ys zs))
    ((fresh (xa xd zd)
       (== xs (cons xa xd))
       (== zs (cons xa zd))
       (appendo xd ys zd)))))

(time (length (run* (x y) (appendo x y (make-list 500 'a)))))
#|
    2 collections
    0.056761699s elapsed cpu time, including 0.003540122s collecting
    0.056790000s elapsed real time, including 0.003549000s collecting
    14091904 bytes allocated, including 13622912 bytes reclaimed
501
|#

(time (length (run* (x y)
      (evalo-unstaged
       `(letrec ((append (lambda (xs ys)
                           (if (null? xs) ys
                               (cons (car xs) (append (cdr xs) ys))))))
          (append ',x ',y))
       (make-list 500 'a)))))
#|
    540 collections
    0.946729815s elapsed cpu time, including 0.079432237s collecting
    0.947504000s elapsed real time, including 0.080120000s collecting
    4532938672 bytes allocated, including 5293984112 bytes reclaimed
501
|#

(time (length (run* (xs ys)
      (u-eval-expo
       `(letrec ((append (lambda (xs ys)
                           (if (null? xs) ys
                               (cons (car xs) (append (cdr xs) ys))))))
          (append xs ys))
       `((xs . (val . ,xs)) (ys . (val . ,ys)) . ,initial-env)
       (make-list 500 'a)))))
#|
(time (length (run* (...) ...)))
    543 collections
    0.954035788s elapsed cpu time, including 0.052852921s collecting
    0.954492000s elapsed real time, including 0.053598000s collecting
    4556261088 bytes allocated, including 4556667088 bytes reclaimed
501
|#

(define-staged-relation (appendo2 xs ys zs)
  (evalo-staged
   `(letrec ((append
              (lambda (xs ys)
                (if (null? xs)
                    ys
                    (cons (car xs)
                          (append (cdr xs) ys))))))
      (append ',xs ',ys))
   zs))

(time (length (run* (x y) (appendo2 x y (make-list 500 'a)))))
#|
    121 collections
    0.892156446s elapsed cpu time, including 0.055932466s collecting
    0.893391000s elapsed real time, including 0.056125000s collecting
    1014630304 bytes allocated, including 1007363808 bytes reclaimed
501
|#

(define-staged-relation (appendo3 xs ys zs)
  (eval-expo
   #t
   `(letrec ((append
              (lambda (xs ys)
                (if (null? xs)
                    ys
                    (cons (car xs)
                          (append (cdr xs) ys))))))
      (append xs ys))
  `((xs . (val . ,xs)) (ys . (val . ,ys)) . ,initial-env)
   zs))

(time (length (run* (x y) (appendo3 x y (make-list 500 'a)))))
#|
(time (length (run* (...) ...)))
    4 collections
    0.143551118s elapsed cpu time, including 0.004270349s collecting
    0.143615000s elapsed real time, including 0.004277000s collecting
    30508768 bytes allocated, including 31886048 bytes reclaimed
501
|#

(define-staged-relation (context-appendo e xs ys res)
  (eval-expo
   #t
   `(letrec ((append
              (lambda (xs ys)
                (if (null? xs)
                    ys
                    (cons (car xs)
                          (append (cdr xs) ys))))))
      ,e)
   `((xs . (val . ,xs)) (ys . (val . ,ys)) . ,initial-env)
   res))

(time (length (run* (xs ys) (context-appendo
                    `(append xs ys)
                    xs ys
                    (make-list 500 'a)))))
#|
(time (length (run* (...) ...)))
    4 collections
    0.147102870s elapsed cpu time, including 0.005187957s collecting
    0.147106000s elapsed real time, including 0.005195000s collecting
    29973712 bytes allocated, including 30532976 bytes reclaimed
501
|#
