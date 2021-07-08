(load "staged-load.scm")

(display "in mk")
(newline)
(define (appendo xs ys zs)
  (conde
    ((== xs '()) (== ys zs))
    ((fresh (xa xd zd)
       (== xs (cons xa xd))
       (== zs (cons xa zd))
       (appendo xd ys zd)))))

(time (length (run* (x y) (appendo x y (make-list 500 'a)))))

(display "unstaged")
(newline)
(time (length (run* (x y)
      (evalo-unstaged
       `(letrec ((append (lambda (xs ys)
                           (if (null? xs) ys
                               (cons (car xs) (append (cdr xs) ys))))))
          (append ',x ',y))
       (make-list 500 'a)))))

(display "unstaged env-passing")
(newline)
(time (length (run* (xs ys)
      (u-eval-expo
       `(letrec ((append (lambda (xs ys)
                           (if (null? xs) ys
                               (cons (car xs) (append (cdr xs) ys))))))
          (append xs ys))
       `((xs . (val . ,xs)) (ys . (val . ,ys)) . ,initial-env)
       (make-list 500 'a)))))

(display "staged")
(newline)
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

(display "staged env-passing")
(newline)
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
