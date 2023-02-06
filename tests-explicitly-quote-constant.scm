;; Adapted by Kanae Tsushima and Will Byrd from Indiana University's
;; old (~2004) compilers course exercise by R. Kent Dyvig, Daniel
;; P. Friedman, and Oscar Waddell.

;; Here we are trying to synthesize a simple pass that ensures
;; constants (booleans and numbers) are explicitly quoted.

(define Expr+call-lambda-args/body-synthesis
  (lambda (q1 q2)
    `(letrec ((Expr
               (lambda (expr)
                 (match expr
                   [`#f
                    (list 'quote #f)]
                   [`#t
                    (list 'quote #t)]
                   [`,(? number? n)
                    (list 'quote n)]
                   [`,(? symbol? var)
                    var]
                   [`(quote ,datum)
                    (list 'quote datum)]
                   [`(if ,test ,conseq ,altern)
                    (list 'if
                          (Expr test)
                          (Expr conseq)
                          (Expr altern))]
                   [`(lambda (,(? symbol? x)) ,body)
                    (list 'lambda ,q1 ,q2)]
                   [`(,rator ,rand)
                    (list (Expr rator) (Expr rand))]))))
       (list (Expr '#t)
             (Expr '#f)
             (Expr '5)
             (Expr '(quote 4))
             (Expr '(4 5))
             (Expr '(#t #f))
             (Expr '(lambda (y) (3 #f)))
             (Expr '(lambda (z) (z (2 5))))
             (Expr '(lambda (w) (#t (7 8))))
             (Expr '(lambda (a) (#f #t)))
             (Expr '(if 5 6 (quote 7)))))))

(define Expr+call-lambda-synthesis
  (lambda (q)
    `(letrec ((Expr
               (lambda (expr)
                 (match expr
                   [`#f
                    (list 'quote #f)]
                   [`#t
                    (list 'quote #t)]
                   [`,(? number? n)
                    (list 'quote n)]
                   [`,(? symbol? var)
                    var]
                   [`(quote ,datum)
                    (list 'quote datum)]
                   [`(if ,test ,conseq ,altern)
                    (list 'if
                          (Expr test)
                          (Expr conseq)
                          (Expr altern))]
                   [`(lambda (,(? symbol? x)) ,body)
                    (list 'lambda . ,q)]
                   [`(,rator ,rand)
                    (list (Expr rator) (Expr rand))]))))
       (list (Expr '#t)
             (Expr '#f)
             (Expr '5)
             (Expr '(quote 4))
             (Expr '(4 5))
             (Expr '(#t #f))
             (Expr '(lambda (y) (3 #f)))
             (Expr '(lambda (z) (z (2 5))))
             (Expr '(lambda (w) (#t (7 8))))
             (Expr '(lambda (a) (#f #t)))
             (Expr '(if 5 6 (quote 7)))))))



(define Expr+call
  `(letrec ((Expr
             (lambda (expr)
               (match expr
                 [`#f
                  (list 'quote #f)]
                 [`#t
                  (list 'quote #t)]
                 [`,(? number? n)
                  (list 'quote n)]
                 [`,(? symbol? var)
                    var]
                 [`(quote ,datum)
                  (list 'quote datum)]
                 [`(if ,test ,conseq ,altern)
                  (list 'if
                        (Expr test)
                        (Expr conseq)
                        (Expr altern))]
                 [`(lambda (,(? symbol? x)) ,body)
                  (list 'lambda (list x) (Expr body))]
                 [`(,rator ,rand)
                  (list (Expr rator) (Expr rand))]))))
     (list (Expr '#t)
           (Expr '#f)
           (Expr '5)
           (Expr '(quote 4))
           (Expr '(4 5))
           (Expr '(#t #f))
           (Expr '(lambda (y) (3 #f)))
           (Expr '(lambda (z) (z (2 5))))
           (Expr '(lambda (w) (#t (7 8))))
           (Expr '(lambda (a) (#f #t)))
           (Expr '(if 5 6 (quote 7))))))



(record-bench 'run-staged 'test-explicitly-quote-constant 1)
(time-test
  (run-staged 1 (q)
    (evalo-staged
     Expr+call
     q))
  '(('#t
     '#f
     '5
     '4
     ('4 '5)
     ('#t '#f)
     (lambda (y) ('3 '#f))
     (lambda (z) (z ('2 '5)))
     (lambda (w) ('#t ('7 '8)))
     (lambda (a) ('#f '#t))
     (if '5 '6 '7))))

(record-bench 'unstaged 'test-explicitly-quote-constant 1)
(time-test
  (run 1 (q)
    (evalo-unstaged
     Expr+call
     q))
  '(('#t
     '#f
     '5
     '4
     ('4 '5)
     ('#t '#f)
     (lambda (y) ('3 '#f))
     (lambda (z) (z ('2 '5)))
     (lambda (w) ('#t ('7 '8)))
     (lambda (a) ('#f '#t))
     (if '5 '6 '7))))


(record-bench 'run-staged 'test-explicitly-quote-constant-lambda-args/body-synthesis 1)
(time-test
  (run-staged 1 (q1 q2)
    (evalo-staged
     (Expr+call-lambda-args/body-synthesis q1 q2)
     '('#t
       '#f
       '5
       '4
       ('4 '5)
       ('#t '#f)
       (lambda (y) ('3 '#f))
       (lambda (z) (z ('2 '5)))
       (lambda (w) ('#t ('7 '8)))
       (lambda (a) ('#f '#t))
       (if '5 '6 '7))))
  '(((cons x '()) (Expr body))))

#|
;; takes too long to run -- doesn't return after a couple of minutes
(record-bench 'unstaged 'test-explicitly-quote-constant-lambda-args/body-synthesis 1)
(time-test
  (run 1 (q1 q2)
    (evalo-unstaged
     (Expr+call-lambda-args/body-synthesis q1 q2)
     '('#t
       '#f
       '5
       '4
       ('4 '5)
       ('#t '#f)
       (lambda (y) ('3 '#f))
       (lambda (z) (z ('2 '5)))
       (lambda (w) ('#t ('7 '8)))
       (lambda (a) ('#f '#t))
       (if '5 '6 '7))))
  '(((cons x '()) (Expr body))))
|#

#|
;; too slow -- doesn't return after several minutes
(record-bench 'run-staged 'test-explicitly-quote-constant-lambda-synthesis 1)
(time-test
  (run-staged 1 (q)
    (evalo-staged
     (Expr+call-lambda-synthesis q)
     '('#t
       '#f
       '5
       '4
       ('4 '5)
       ('#t '#f)
       (lambda (y) ('3 '#f))
       (lambda (z) (z ('2 '5)))
       (lambda (w) ('#t ('7 '8)))
       (lambda (a) ('#f '#t))
       (if '5 '6 '7))))
  '(((cons x '()) (Expr body))))
|#

#|
;; too slow -- doesn't return after several minutes
(record-bench 'unstaged 'test-explicitly-quote-constant-lambda-synthesis 1)
(time-test
  (run 1 (q)
    (evalo-unstaged
     (Expr+call-lambda-synthesis q)
     '('#t
       '#f
       '5
       '4
       ('4 '5)
       ('#t '#f)
       (lambda (y) ('3 '#f))
       (lambda (z) (z ('2 '5)))
       (lambda (w) ('#t ('7 '8)))
       (lambda (a) ('#f '#t))
       (if '5 '6 '7))))
  '(('lambda '(y) '('3 '#f))))
|#
