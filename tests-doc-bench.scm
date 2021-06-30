(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(load "staged-interp.scm")
(load "staged-utils.scm")
(load "staged-run.scm")

(load "unstaged-interp.scm")

(load "test-check.scm")

(define (record-bench phase name . args)
  (if (null? args)
      (printf "BENCH ~a ~a\n" phase name)
      (printf "BENCH ~a ~a ~a\n" phase name (car args))))



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

(record-bench 'run-unstaged 'appendo-synth-0)
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

;; WEB: this seems very slow!  I'm surprised.
;;
;; We really need to be sure the staged and unstaged interpreters are identical, as much as possible.
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


;; WEB Seems like the evaluator biases towards using `match` rather than using `cdr`.
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

;; WEB: hopelessly slow
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


;;; WEB: didn't return after several minutes
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


