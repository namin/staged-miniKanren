(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(load "staged-interp.scm")
(load "staged-utils.scm")
(load "staged-run.scm")

(load "unstaged-interp.scm")

(load "test-check.scm")

;; Synthesizes a 'match'-based version of 'null?'
(record-bench 'run-staged 'appendo-synth-0)
(time-test
  (run-staged 1 (q)
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
  (run-staged 1 (q)
    (absento 'match q)
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
       (c d e f))))
  '((equal? xs '())))


;;  Even slower to generate the `null?` version of the test, if `match` and `equal?` are disallowed.
(record-bench 'run-staged 'appendo-synth-0c)
(time-test
  (run-staged 1 (q)
    (absento 'match q)
    (absento 'equal? q)
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
       (c d e f))))
  '((null? xs)))

;;  if we don't exclude `match`, variants of `match` will be generated with a `run 3`, rather than `null?`
(record-bench 'run-staged 'appendo-synth-0d)
(time-test
  (run-staged 3 (q)
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
       (c d e f))))
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
  (run-staged 1 (q)
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
       (c d e f))))
  '((car xs)))

;;  this seems very slow!  I'm surprised.
;;
;; We really need to be sure the staged and unstaged interpreters are identical, as much as possible.
#|
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
|#

;;  Seems like the evaluator biases towards using `match` rather than using `cdr`.
;; `(cdr xs)` was the code that was removed.  The synthesized `match` also works, however.
(record-bench 'run-staged 'appendo-synth-2)
(time-test
  (run-staged 1 (q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (absento 'f q)
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

;;  hopelessly slow
#|
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
|#


(record-bench 'run-staged 'appendo-synth-2b)
(time-test
  (run-staged 1 (q)
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
       (g h i j k l))))
  '(((match rest
    [`() ys]
    [`(,_.0) (cons _.0 ys)]
    [`(,_.1 ,_.2) (cons _.1 (cons _.2 ys))]
    .
    _.3)
   $$
   (=/= ((_.0 a)) ((_.0 b)) ((_.0 c)) ((_.0 cons)) ((_.0 d))
    ((_.0 e)) ((_.0 f)) ((_.0 g)) ((_.0 h)) ((_.0 i)) ((_.0 j))
    ((_.0 k)) ((_.0 l)) ((_.0 ys)) ((_.1 _.2)) ((_.1 a))
    ((_.1 b)) ((_.1 c)) ((_.1 cons)) ((_.1 d)) ((_.1 e))
    ((_.1 f)) ((_.1 g)) ((_.1 h)) ((_.1 i)) ((_.1 j)) ((_.1 k))
    ((_.1 l)) ((_.1 ys)) ((_.2 a)) ((_.2 b)) ((_.2 c))
    ((_.2 cons)) ((_.2 d)) ((_.2 e)) ((_.2 f)) ((_.2 g))
    ((_.2 h)) ((_.2 i)) ((_.2 j)) ((_.2 k)) ((_.2 l))
    ((_.2 ys)))
   (sym _.0 _.1 _.2)
   (absento (a _.3) (b _.3) (c _.3) (d _.3) (e _.3) (f _.3)
     (g _.3) (h _.3) (i _.3) (j _.3) (k _.3) (l _.3)))))



;;;  didn't return after several minutes
#|
(record-bench 'run-staged 'appendo-synth-3)
(time-test
  (run-staged 1 (q)
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
       (g h i j k l))))
  '((append (cdr xs) ys)))
|#


(record-bench 'run-staged 'appendo-tail)
(time-test
 (length
  (run-staged 50 (q)
    (evalo-staged
     `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys
                      (cons (car xs) (append (cdr xs) ys))))))
        (append '(1 2) ,q))
     '(1 2 3 4))))
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
 (run-staged #f (q)
    (evalo-staged
     `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys
                      (cons (car xs) (append (cdr xs) ys))))))
        (append '(1 2) ',q))
     '(1 2 3 4)))
 '((3 4)))

(record-bench 'unstaged 'appendo-tail-quoted)
(time-test
  (run #f (q)
    (evalo-unstaged
     `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys
                      (cons (car xs) (append (cdr xs) ys))))))
        (append '(1 2) ',q))
     '(1 2 3 4)))
  '((3 4)))
