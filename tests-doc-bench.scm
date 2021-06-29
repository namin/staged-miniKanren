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


#!eof

