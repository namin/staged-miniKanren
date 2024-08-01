#lang racket/base

(require "../all.rkt")

;; Synthesizes a 'match'-based version of 'null?'
(record-bench 'run-staged 'appendo-synth-0)
(time-test
 (run 1 (q)
   (staged
    (evalo-staged
     `(letrec ((append
                (lambda (xs ys)
                  (if ,q ys
                      (cons (car xs) (append (cdr xs) ys))))))
        (list (append '() '())
              (append '(a) '(b))
              (append '(c d) '(e f))))
     '(()
       (a b)
       (c d e f)))))
  '(((match xs
       [`() _.0]
       [_.1 '#f]
       .
       _.2)
     $$
     (=/= ((_.1 quote)))
     (num _.0)
     (sym _.1))))

(record-bench 'unstaged 'appendo-synth-0)
(time-test
  (run 1 (q)
    (evalo-unstaged
     `(letrec ((append
                (lambda (xs ys)
                  (if ,q ys
                      (cons (car xs) (append (cdr xs) ys))))))
        (list (append '() '())
              (append '(a) '(b))
              (append '(c d) '(e f))))
     '(()
       (a b)
       (c d e f))))
  '(((match xs
       [`() _.0]
       [_.1 '#f]
       .
       _.2)
     $$
     (=/= ((_.1 quote)))
     (num _.0)
     (sym _.1))))


;;  Much slower to come up with the `equal?` equivalent of `(null? xs)`, if `match` is disallowed.
(record-bench 'run-staged 'appendo-synth-0b)
(time-test
  (run 1 (q)
    (absento 'match q)
    (staged
     (evalo-staged
      `(letrec ((append
                 (lambda (xs ys)
                   (if ,q ys
                       (cons (car xs) (append (cdr xs) ys))))))
         (list (append '() '())
               (append '(a) '(b))
               (append '(c d) '(e f))))
      '(()
        (a b)
        (c d e f)))))
  '((equal? xs '())))


;;  Even slower to generate the `null?` version of the test, if `match` and `equal?` are disallowed.
(record-bench 'run-staged 'appendo-synth-0c)
(time-test
 (run 1 (q)
   (absento 'match q)
   (absento 'equal? q)
   (staged
    (evalo-staged
     `(letrec ((append
                (lambda (xs ys)
                  (if ,q ys
                      (cons (car xs) (append (cdr xs) ys))))))
        (list (append '() '())
              (append '(a) '(b))
              (append '(c d) '(e f))))
     '(()
       (a b)
       (c d e f)))))
 '((null? xs)))

;;  if we don't exclude `match`, variants of `match` will be generated with a `run 3`, rather than `null?`
(record-bench 'run-staged 'appendo-synth-0d)
(todo "TODO(fix): no longer works exactly the same"
 (run 3 (q)
   (staged
    (evalo-staged
     `(letrec ((append
                (lambda (xs ys)
                  (if ,q ys
                      (cons (car xs) (append (cdr xs) ys))))))
        (list (append '() '())
              (append '(a) '(b))
              (append '(c d) '(e f))))
     '(()
       (a b)
       (c d e f)))))
  '(((match xs
       (`() _.0)
       (_.1 '#f)
       .
       _.2)
     $$
     (=/= ((_.1 quote))) (num _.0) (sym _.1))
    ((match xs
       (`() _.0)
       (`,_.1 '#f)
       .
       _.2)
     $$
     (=/= ((_.1 quote))) (num _.0) (sym _.1))
    ((match xs
       (`() _.0)
       (_.1 _.2)
       (_.3 '#f)
       .
       _.4)
     $$
     (=/= ((_.3 quote))) (num _.0 _.1) (sym _.3))))


(record-bench 'unstaged 'appendo-synth-0d)
(time-test
  (run 1 (q)
    (evalo-unstaged
     `(letrec ((append
                (lambda (xs ys)
                  (if ,q ys
                      (cons (car xs) (append (cdr xs) ys))))))
        (list (append '() '())
              (append '(a) '(b))
              (append '(c d) '(e f))))
     '(()
       (a b)
       (c d e f))))
  '(((match xs [`() _.0] [_.1 '#f] . _.2)
     $$
     (=/= ((_.1 quote)))
     (num _.0)
     (sym _.1))))



(record-bench 'run-staged 'appendo-synth-1)
(time-test
 (run 1 (q)
   (staged
    (evalo-staged
     `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys
                      (cons ,q (append (cdr xs) ys))))))
        (list (append '() '())
              (append '(a) '(b))
              (append '(c d) '(e f))))
     '(()
       (a b)
       (c d e f)))))
  '((car xs)))

(record-bench 'unstaged 'appendo-synth-1)
(time-test
  (run 1 (q)
    (evalo-unstaged
     `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys
                      (cons ,q (append (cdr xs) ys))))))
        (list (append '() '())
              (append '(a) '(b))
              (append '(c d) '(e f))))
     '(()
       (a b)
       (c d e f))))
  '((car xs)))


;;  Seems like the evaluator biases towards using `match` rather than using `cdr`.
;; `(cdr xs)` was the code that was removed.  The synthesized `match` also works, however.
(record-bench 'run-staged 'appendo-synth-2)
(time-test
  (run 1 (q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (staged
     (evalo-staged
      `(letrec ((append
                 (lambda (xs ys)
                   (if (null? xs) ys
                       (cons (car xs) (append ,q ys))))))
         (list (append '() '())
               (append '(a) '(b))
               (append '(c d) '(e f))))
      '(()
        (a b)
        (c d e f)))))
  '(((match xs
       (`(,_.0) '())
       (`(,_.1 . ,_.2) _.2)
       .
       _.3)
     $$
     (=/= ((_.0 a))
          ((_.0 b))
          ((_.0 c))
          ((_.0 d))
          ((_.0 e))
          ((_.0 f))
          ((_.0 quote))
          ((_.1 _.2))
          ((_.1 a))
          ((_.1 b))
          ((_.1 c))
          ((_.1 d))
          ((_.1 e))
          ((_.1 f))
          ((_.2 a))
          ((_.2 b))
          ((_.2 c))
          ((_.2 d))
          ((_.2 e))
          ((_.2 f)))
     (sym _.0 _.1 _.2)
     (absento (a _.3) (b _.3) (c _.3) (d _.3) (e _.3) (f _.3)))))


(record-bench 'unstaged 'appendo-synth-2)
(time-test
  (run 1 (q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (evalo-unstaged
     `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys
                      (cons (car xs) (append ,q ys))))))
        (list (append '() '())
              (append '(a) '(b))
              (append '(c d) '(e f))))
     '(()
       (a b)
       (c d e f))))
  '(((match xs
       (`(,_.0) '())
       (`(,_.1 . ,_.2) _.2)
       .
       _.3)
     $$
     (=/= ((_.0 a))
          ((_.0 b))
          ((_.0 c))
          ((_.0 d))
          ((_.0 e))
          ((_.0 f))
          ((_.0 quote))
          ((_.1 _.2))
          ((_.1 a))
          ((_.1 b))
          ((_.1 c))
          ((_.1 d))
          ((_.1 e))
          ((_.1 f))
          ((_.2 a))
          ((_.2 b))
          ((_.2 c))
          ((_.2 d))
          ((_.2 e))
          ((_.2 f)))
     (sym _.0 _.1 _.2)
     (absento (a _.3) (b _.3) (c _.3) (d _.3) (e _.3) (f _.3)))))



(record-bench 'run-staged 'appendo-synth-2b)
(time-test
  (run 1 (q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
    (absento 'g q)
    (absento 'h q)
    (absento 'i q)
    (absento 'j q)
    (absento 'k q)
    (absento 'l q)
    (staged
     (evalo-staged
      `(letrec ((append
                 (lambda (xs ys)
                   (match xs
                     [`() ys]
                     [`(,x . ,rest) (cons x ,q)]))))
         (list (append '() '())
               (append '(a) '(b))
               (append '(c d) '(e f))
               (append '(g h i) '(j k l))))
      '(()
        (a b)
        (c d e f)
        (g h i j k l)))))
  '((append rest ys)))


(record-bench 'run-staged 'appendo-synth-3)
(time-test
 (run 1 (q)
   (absento 'a q)
   (absento 'b q)
   (absento 'c q)
   (absento 'd q)
   (absento 'e q)
   (absento 'f q)
   (absento 'g q)
   (absento 'h q)
   (absento 'i q)
   (absento 'j q)
   (absento 'k q)
   (absento 'l q)
   (staged
    (evalo-staged
     `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys
                      (cons (car xs) ,q)))))
        (list (append '() '())
              (append '(a) '(b))
              (append '(c d) '(e f))
              (append '(g h i) '(j k l))))
     '(()
       (a b)
       (c d e f)
       (g h i j k l)))))
 ;; We would have liked this:
 #;'((append (cdr xs) ys))
 ;; But the result we get seems correct, too:
 '(((match
    xs
    (`(,_.0) ys)
    (`(,_.1 ,_.2) (cons _.2 ys))
    (`(,_.3 unquote _.4) (append _.4 ys))
    .
    _.5)
   $$
   (=/=
    ((_.0 a))
    ((_.0 b))
    ((_.0 c))
    ((_.0 d))
    ((_.0 e))
    ((_.0 f))
    ((_.0 g))
    ((_.0 h))
    ((_.0 i))
    ((_.0 j))
    ((_.0 k))
    ((_.0 l))
    ((_.0 ys))
    ((_.1 _.2))
    ((_.1 a))
    ((_.1 b))
    ((_.1 c))
    ((_.1 cons))
    ((_.1 d))
    ((_.1 e))
    ((_.1 f))
    ((_.1 g))
    ((_.1 h))
    ((_.1 i))
    ((_.1 j))
    ((_.1 k))
    ((_.1 l))
    ((_.1 ys))
    ((_.2 a))
    ((_.2 b))
    ((_.2 c))
    ((_.2 cons))
    ((_.2 d))
    ((_.2 e))
    ((_.2 f))
    ((_.2 g))
    ((_.2 h))
    ((_.2 i))
    ((_.2 j))
    ((_.2 k))
    ((_.2 l))
    ((_.2 ys))
    ((_.3 _.4))
    ((_.3 a))
    ((_.3 append))
    ((_.3 b))
    ((_.3 c))
    ((_.3 d))
    ((_.3 e))
    ((_.3 f))
    ((_.3 g))
    ((_.3 h))
    ((_.3 i))
    ((_.3 j))
    ((_.3 k))
    ((_.3 l))
    ((_.3 ys))
    ((_.4 a))
    ((_.4 append))
    ((_.4 b))
    ((_.4 c))
    ((_.4 d))
    ((_.4 e))
    ((_.4 f))
    ((_.4 g))
    ((_.4 h))
    ((_.4 i))
    ((_.4 j))
    ((_.4 k))
    ((_.4 l))
    ((_.4 ys)))
   (sym _.0 _.1 _.2 _.3 _.4)
   (absento
    (a _.5)
    (b _.5)
    (c _.5)
    (d _.5)
    (e _.5)
    (f _.5)
    (g _.5)
    (h _.5)
    (i _.5)
    (j _.5)
    (k _.5)
    (l _.5)))))

;; The unstaged version didn't come back after several minutes.
#|
(record-bench 'unstaged 'appendo-synth-3)
(time-test
 (run 1 (q)
   (absento 'a q)
   (absento 'b q)
   (absento 'c q)
   (absento 'd q)
   (absento 'e q)
   (absento 'f q)
   (absento 'g q)
   (absento 'h q)
   (absento 'i q)
   (absento 'j q)
   (absento 'k q)
   (absento 'l q)
   
   (evalo-unstaged
    `(letrec ((append
               (lambda (xs ys)
                 (if (null? xs) ys
                     (cons (car xs) ,q)))))
       (list (append '() '())
             (append '(a) '(b))
             (append '(c d) '(e f))
             (append '(g h i) '(j k l))))
    '(()
      (a b)
      (c d e f)
      (g h i j k l))))
 '(((match
        xs
      (`(,_.0) ys)
      (`(,_.1 ,_.2) (cons _.2 ys))
      (`(,_.3 unquote _.4) (append _.4 ys))
      .
      _.5)
    $$
    (=/=
     ((_.0 a))
     ((_.0 b))
     ((_.0 c))
     ((_.0 d))
     ((_.0 e))
     ((_.0 f))
     ((_.0 g))
     ((_.0 h))
     ((_.0 i))
     ((_.0 j))
     ((_.0 k))
     ((_.0 l))
     ((_.0 ys))
     ((_.1 _.2))
     ((_.1 a))
     ((_.1 b))
     ((_.1 c))
     ((_.1 cons))
     ((_.1 d))
     ((_.1 e))
     ((_.1 f))
     ((_.1 g))
     ((_.1 h))
     ((_.1 i))
     ((_.1 j))
     ((_.1 k))
     ((_.1 l))
     ((_.1 ys))
     ((_.2 a))
     ((_.2 b))
     ((_.2 c))
     ((_.2 cons))
     ((_.2 d))
     ((_.2 e))
     ((_.2 f))
     ((_.2 g))
     ((_.2 h))
     ((_.2 i))
     ((_.2 j))
     ((_.2 k))
     ((_.2 l))
     ((_.2 ys))
     ((_.3 _.4))
     ((_.3 a))
     ((_.3 append))
     ((_.3 b))
     ((_.3 c))
     ((_.3 d))
     ((_.3 e))
     ((_.3 f))
     ((_.3 g))
     ((_.3 h))
     ((_.3 i))
     ((_.3 j))
     ((_.3 k))
     ((_.3 l))
     ((_.3 ys))
     ((_.4 a))
     ((_.4 append))
     ((_.4 b))
     ((_.4 c))
     ((_.4 d))
     ((_.4 e))
     ((_.4 f))
     ((_.4 g))
     ((_.4 h))
     ((_.4 i))
     ((_.4 j))
     ((_.4 k))
     ((_.4 l))
     ((_.4 ys)))
    (sym _.0 _.1 _.2 _.3 _.4)
    (absento
     (a _.5)
     (b _.5)
     (c _.5)
     (d _.5)
     (e _.5)
     (f _.5)
     (g _.5)
     (h _.5)
     (i _.5)
     (j _.5)
     (k _.5)
     (l _.5)))))
|#


(record-bench 'run-staged 'appendo-tail)
(time-test
 (length
  (run 50 (q)
    (staged
     (evalo-staged
      `(letrec ((append
                 (lambda (xs ys)
                   (if (null? xs) ys
                       (cons (car xs) (append (cdr xs) ys))))))
         (append '(1 2) ,q))
      '(1 2 3 4)))))
 50)

(record-bench 'unstaged 'appendo-tail)
(time-test
  (length
   (run 50 (q)
     (evalo-unstaged
      `(letrec ((append
                 (lambda (xs ys)
                   (if (null? xs) ys
                       (cons (car xs) (append (cdr xs) ys))))))
         (append '(1 2) ,q))
      '(1 2 3 4))))
  50)




(record-bench 'run-staged 'appendo-tail-quoted)
(time-test
 (run* (q)
   (staged
    (evalo-staged
     `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys
                      (cons (car xs) (append (cdr xs) ys))))))
        (append '(1 2) ',q))
     '(1 2 3 4))))
 '((3 4)))

(record-bench 'unstaged 'appendo-tail-quoted)
(time-test
  (run* (q)
    (evalo-unstaged
     `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys
                      (cons (car xs) (append (cdr xs) ys))))))
        (append '(1 2) ',q))
     '(1 2 3 4)))
  '((3 4)))

(record-bench 'run-staged 'map-hole 0)
(time-test
 (run 1 (q)
   (staged
    (evalo-staged
     `(letrec ((map (lambda (f l)
                      (if (null? l)
                          '()
                          (cons (f (car l))
                                (map f (cdr l)))))))
        (map (lambda (x) ,q) '(a b c)))
     '((a . a) (b . b) (c . c)))))
 '((cons x x)))

;; u-eval-expo seems 50% faster than the staged version
;; (record-bench 'unstaged 'map-hole 0)
;; (time-test
;;   (run 1 (q)
;;     (u-eval-expo
;;      `(letrec ((map (lambda (f l)
;;                       (if (null? l)
;;                           '()
;;                           (cons (f (car l))
;;                                 (map f (cdr l)))))))
;;         (map (lambda (x) ,q) '(a b c)))
;;      initial-env
;;      '((a . a) (b . b) (c . c))))
;;   '((cons x x)))

;; (record-bench 'run-staged 'map-hole 1)
;; (time-test
;;  (syn-hole 1
;;    (lambda (q)
;;      `(letrec ((map (lambda (f l)
;;                       (if (null? l)
;;                           '()
;;                           (cons (f (car l))
;;                                 (map f (cdr l)))))))
;;         (map (lambda (x) ,q) '(a b c))))
;;    '((a (a) a) (b (b) b) (c (c) c))
;;    (lambda (q) (absento 'a q)))
;;  '((list x (list x) x)))

;; (record-bench 'unstaged 'map-hole 1)
;; (time-test
;;   (run 1 (q)
;;     (absento 'a q)
;;     (u-eval-expo
;;      `(letrec ((map (lambda (f l)
;;                       (if (null? l)
;;                           '()
;;                           (cons (f (car l))
;;                                 (map f (cdr l)))))))
;;         (map (lambda (x) ,q) '(a b c)))
;;      initial-env
;;      '((a (a) a) (b (b) b) (c (c) c))))
;;  '((list x (list x) x)))
