#lang racket

(require "../../../all.rkt")

(defrel (appendo xs ys zs)
  (conde
    [(== xs '()) (== ys zs)]
    [(fresh (xa xd zd)
       (== xs (cons xa xd))
       (== zs (cons xa zd))
       (appendo xd ys zd))]))

(run* (xs ys) (appendo xs ys '(a b c)))

(time (for ([i 1000]) (run* (xs ys) (appendo xs ys '(a b c)))))

(defrel (appendo-via-unstaged-interp xs ys zs)
  (evalo-unstaged
   `(letrec
        ([append
          (lambda (xs ys)
            (if (null? xs) ys
                (cons (car xs)
                      (append (cdr xs) ys))))])
      (append ',xs ',ys))
   zs))

(defrel (appendo-via-staged-interp xs ys zs)
  (staged
   (evalo-staged
    `(letrec
         ([append
           (lambda (xs ys)
             (if (null? xs) ys
                 (cons (car xs)
                       (append (cdr xs) ys))))])
       (append ',xs ',ys))
    zs)))
(generated-code)

(time (for ([i 1000]) (run* (xs ys) (appendo-via-unstaged-interp xs ys '(a b c)))))
(time (for ([i 1000]) (run* (xs ys) (appendo-via-staged-interp xs ys '(a b c)))))
