(load "staged-load.scm")

(define (micro0 query)
     `(letrec
       ((assp
         (lambda (p l)
           (if (null? l) #f
               (if (p (car (car l))) (car l)
                   (assp p (cdr l))))))

        (var
         (lambda (c) (cons 'var c)))
        (var?
         (lambda (x) (and (pair? x) (equal? (car x) 'var))))
        (var=?
         (lambda (x1 x2) (equal? (cdr x1) (cdr x2))))

        (walk
         (lambda (u s)
           ((lambda (pr) (if pr (walk (cdr pr) s) u))
            (and (var? u) (assp (lambda (v) (var=? u v)) s)))))

        (ext-s
         (lambda (x v s)
           (cons (cons x v) s)))

        (===
         (lambda (u v)
           (lambda (s/c)
             ((lambda (s) (if s (unit (cons s (cdr s/c))) (mzero)))
              (unify u v (car s/c))))))

        (unit
         (lambda (s/c)
           (cons s/c (mzero))))
        (mzero
         (lambda ()
           '()))

        (unify
         (lambda (u v s)
           ((lambda (u v)
              (if (and (var? u) (var? v) (var=? u v)) s
                  (if (var? u) (ext-s u v s)
                      (if (var? v) (ext-s v u s)
                          (if (and (pair? u) (pair? v))
                              ((lambda (s) (and s (unify (cdr u) (cdr v) s)))
                               (unify (car u) (car v) s))
                              (and (equal? u v) s))))))
            (walk u s) (walk v s))))

        (call/fresh
         (lambda (f)
           (lambda (s/c)
             ((lambda (c) ((f (var c)) (cons (car s/c) (cons 's c))))
              (cdr s/c)))))

        (disj
         (lambda (g1 g2)
           (lambda (s/c) (mplus (g1 s/c) (g2 s/c)))))
        (conj
         (lambda (g1 g2)
           (lambda (s/c) (bind (g1 s/c) g2))))

        (mplus
         (lambda ($1 $2)
           (if (null? $1) $2
               (if (pair? $1) (cons (car $1) (mplus (cdr $1) $2))
                   (lambda () (mplus $2 ($1)))))))

        (bind
         (lambda ($ g)
           (if (null? $) (mzero)
               (if (pair? $) (mplus (g (car $)) (bind (cdr $) g))
                   (lambda () (bind ($) g))))))

        (empty-state
         (lambda ()
           '(() . z)))

        (pull
         (lambda ($)
           (if (or (null? $) (pair? $)) $ (pull ($)))))

        (take-all
         (lambda ($)
           ((lambda ($) (if (null? $) '() (cons (car $) (take-all (cdr $)))))
            (pull $))))

        (take
         (lambda (n $)
           (if (equal? n 'z) '()
               ((lambda ($) (if (null? $) '() (cons (car $) (take (cdr n) (cdr $)))))
                (pull $)))))
)

     ,query

     ))

(define (micro query)
     `(letrec
       ((assp
         (lambda (p l)
           (if (null? l) #f
               (if (p (car (car l))) (car l)
                   (assp p (cdr l)))))))

       (letrec ((var
         (lambda (c) (cons 'var c))))
         (letrec ((var?
                   (lambda (x) (and (pair? x) (equal? (car x) 'var)))))
           (letrec ((var=?
                     (lambda (x1 x2) (equal? (cdr x1) (cdr x2)))))
             (letrec ((walk
                       (lambda (u s)
                         ((lambda (pr) (if pr (walk (cdr pr) s) u))
                          (and (var? u) (assp (lambda (v) (var=? u v)) s))))))
               (letrec ((ext-s
                         (lambda (x v s)
                           (cons (cons x v) s))))
                 (letrec ((mzero
                             (lambda ()
                               '())))
                   (letrec ((unit
                             (lambda (s/c)
                               (cons s/c (mzero)))))
                     (letrec ((unify
                               (lambda (u v s)
                                 ((lambda (u v)
                                    (if (and (var? u) (var? v) (var=? u v)) s
                                        (if (var? u) (ext-s u v s)
                                            (if (var? v) (ext-s v u s)
                                                (if (and (pair? u) (pair? v))
                                                    ((lambda (s) (and s (unify (cdr u) (cdr v) s)))
                                                     (unify (car u) (car v) s))
                                                    (and (equal? u v) s))))))
                                  (walk u s) (walk v s)))))
                       (letrec ((===
                                 (lambda (u v)
                                   (lambda (s/c)
                                     ((lambda (s) (if s (unit (cons s (cdr s/c))) (mzero)))
                                      (unify u v (car s/c)))))))
                         (letrec ((call/fresh
                                   (lambda (f)
                                     (lambda (s/c)
                                       ((lambda (c) ((f (var c)) (cons (car s/c) (cons 's c))))
                                        (cdr s/c))))))
                           (letrec ((mplus
                                     (lambda ($1 $2)
                                       (if (null? $1) $2
                                           (if (pair? $1) (cons (car $1) (mplus (cdr $1) $2))
                                               (lambda () (mplus $2 ($1))))))))

                             (letrec ((bind
                                       (lambda ($ g)
                                         (if (null? $) (mzero)
                                             (if (pair? $) (mplus (g (car $)) (bind (cdr $) g))
                                                 (lambda () (bind ($) g)))))))

                               (letrec ((disj
                                         (lambda (g1 g2)
                                           (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))))
                                 (letrec ((conj
                                           (lambda (g1 g2)
                                             (lambda (s/c) (bind (g1 s/c) g2)))))
                                   (letrec ((empty-state
                                             (lambda ()
                                               '(() . z))))

                                     (letrec ((pull
                                               (lambda ($)
                                                 (if (or (null? $) (pair? $)) $ (pull ($))))))
                                       (letrec ((take-all
                                                 (lambda ($)
                                                   ((lambda ($) (if (null? $) '() (cons (car $) (take-all (cdr $)))))
                                                    (pull $)))))

                                         (letrec ((take
                                                   (lambda (n $)
                                                     (if (equal? n 'z) '()
                                                         ((lambda ($) (if (null? $) '() (cons (car $) (take (cdr n) (cdr $)))))
                                                          (pull $))))))

                                         ,query

                                         ))))))))))))))))))))


(define (micro-unstaged query)
  (eval (micro `(,query (empty-state)))))

(micro-unstaged 'unit)

(record-bench 'run-staged 'micro 0)
(time-test
  (run-staged #f (v)
    (evalo-staged
     (micro '((=== 5 5) (empty-state)))
     v))
  '(((() . z) . ())))

(record-bench 'unstaged 'micro 0)
(time-test
  (run #f (v)
    (evalo-unstaged
     (micro '((=== 5 5) (empty-state)))
     v))
  '(((() . z) . ())))


(record-bench 'run-staged 'micro 1)
(time-test
  (run-staged #f (v)
    (evalo-staged
     (micro '((call/fresh (lambda (q) (=== q 5))) (empty-state)))
     v))
  '((((((var . z) . 5)) . (s . z)))))

(record-bench 'unstaged 'micro 1)
(time-test
  (run #f (v)
    (evalo-unstaged
     (micro '((call/fresh (lambda (q) (=== q 5))) (empty-state)))
     v))
  '((((((var . z) . 5)) . (s . z)))))


(record-bench 'run-staged 'micro 2)
(time-test
  (run-staged #f (v)
    (evalo-staged
     (micro '((call/fresh (lambda (q) (disj (=== q 5) (=== q 6)))) (empty-state)))
     v))
  '((((((var . z) . 5)) . (s . z))
     ((((var . z) . 6)) . (s . z)))))

(record-bench 'unstaged 'micro 2)
(time-test
  (run #f (v)
    (evalo-unstaged
     (micro '((call/fresh (lambda (q) (disj (=== q 5) (=== q 6)))) (empty-state)))
     v))
  '((((((var . z) . 5)) . (s . z))
     ((((var . z) . 6)) . (s . z)))))



;; This generates answers that are not valid microKanren programs.
;; TODO: we lost unit
(test
    (run-staged 3 (q)
      (evalo-staged
       (micro `(,q (empty-state)))
       '((() . z))))
  '(list
    ((lambda _.0 _.0) $$ (sym _.0))
    ((lambda _.0 '((() . z))) $$ (=/= ((_.0 quote))) (sym _.0))))

(test
    (run 3 (q)
      (evalo-unstaged
       (micro `(,q (empty-state)))
       '((() . z))))
  '(unit list ((lambda _.0 _.0) $$ (sym _.0))))


(define (valid-ge? ge)
  `(letrec
       ((valid-te? (lambda (te)
                     (match te
                       ;; symbols, numbers, and booleans must be quoted
                       [`(quote ,datum) #t]
                       [`(cons ,te1 ,te2)
                        (and (valid-te? te1) (valid-te? te2))]
                       [`,else #f]))))
     (letrec ((valid-ge? (lambda (ge)
                           (match ge
                             [`(=== ,t1 ,t2) (and (valid-te? t1) (valid-te? t2))]
                             [`(conj ,ge1 ,ge2) (and (valid-ge? ge1) (valid-ge? ge2))]
                             [`(disj ,ge1 ,ge2) (and (valid-ge? ge1) (valid-ge? ge2))]
                             [`(call/fresh (lambda (,(? symbol? x)) ,ge)) (valid-ge? ge)]
                             [`,else #f]))))
       (valid-ge? ',ge))))


(record-bench 'run-staged 'micro 3)
(time-test
  (run-staged 1 (ge)
    (evalo-staged
     (valid-ge? ge)
     #t)
    (evalo-staged
     (micro `(,ge (empty-state)))
     '((() . z) . ())))
  '(((=== '_.0 '_.0)
     $$
     (=/= ((_.0 call)) ((_.0 closure)) ((_.0 dynamic)) ((_.0 prim)))
     (sym _.0))))

(record-bench 'unstaged 'micro 3)
(time-test
  (run 1 (ge)
    (evalo-unstaged
     (valid-ge? ge)
     #t)
    (evalo-unstaged
     (micro `(,ge (empty-state)))
     '((() . z) . ())))
  '(((=== '_.0 '_.0)
     $$
     (=/= ((_.0 call)) ((_.0 closure)) ((_.0 dynamic)) ((_.0 prim)))
     (sym _.0))))


;;;  doesn't come back after a minute
#|
(record-bench 'run-staged 'micro 4)
(time-test
  (run-staged 1 (ge)
    (evalo-staged
     (valid-ge? ge)
     #t)
    (evalo-staged
     (micro `(,ge (empty-state)))
     '(((((var . z) . 5)) s . z)
       ((((var . z) . 6)) s . z))))
  '???)
|#


;;;  doesn't come back after a minute
#|
(test
    (run-staged 1 (ge)
      (evalo-staged
       (micro `(,ge (empty-state)))
       '((() . z)))
      (evalo-staged
       (valid-ge? ge)
       #t))
  '((=== '5 '5)))
|#



(test
    (run-staged #f (v)
      (evalo-staged
       (valid-ge? `(=== '5 '5))
       #t)
      (evalo-staged
       (micro `((=== '5 '5) (empty-state)))
       v))
  '(((() . z))))

(test
    (run-staged #f (ge v)
      (== `(=== '5 '5) ge)
      (evalo-staged
       (valid-ge? ge)
       #t)
      (evalo-staged
       (micro `(,ge (empty-state)))
       v))
  '(((=== '5 '5) ((() . z)))))

(test
    (run-staged #f (ge v)
      (== `(=== '5 '5) ge)
      (evalo-staged
       (micro `(,ge (empty-state)))
       v)
      (evalo-staged
       (valid-ge? ge)
       #t))
  '(((=== '5 '5) ((() . z)))))



(test
    (run-staged 1 (ge)
      (== '(=== '5 '5) ge)
      (evalo-staged
       (micro `(,ge (empty-state)))
       '((() . z)))
      (evalo-staged
       (valid-ge? ge)
       #t))
  '((=== '5 '5)))


(test
    (run-staged #f (v)
      (evalo-staged
       (valid-ge? `(=== '5 '5))
       v))
  '(#t))

(test
    (run* (v)
      (evalo-unstaged
       (valid-ge? `(=== '5 '5))
       v))
  '(#t))


(test
    (run-staged #f (v)
      (evalo-staged
       (valid-ge? `(=== 5 '5))
       v))
  '(#f))

(test
    (run* (v)
      (evalo-unstaged
       (valid-ge? `(=== 5 '5))
       v))
  '(#f))


(test
    (run-staged 11 (q)
      (evalo-staged
       (valid-ge? q)
       #t))
  '(((=== '_.0 '_.1)
     $$
     (absento (call _.0) (call _.1) (closure _.0) (closure _.1)
              (dynamic _.0) (dynamic _.1) (prim _.0) (prim _.1)))
    ((=== '_.0 (cons '_.1 '_.2))
     $$
     (absento (call _.0) (call _.1) (call _.2) (closure _.0)
              (closure _.1) (closure _.2) (dynamic _.0) (dynamic _.1)
              (dynamic _.2) (prim _.0) (prim _.1) (prim _.2)))
    ((=== (cons '_.0 '_.1) '_.2)
     $$
     (absento (call _.0) (call _.1) (call _.2) (closure _.0)
              (closure _.1) (closure _.2) (dynamic _.0) (dynamic _.1)
              (dynamic _.2) (prim _.0) (prim _.1) (prim _.2)))
    ((conj (=== '_.0 '_.1) (=== '_.2 '_.3))
     $$
     (absento (call _.0) (call _.1) (call _.2) (call _.3) (closure _.0)
              (closure _.1) (closure _.2) (closure _.3) (dynamic _.0)
              (dynamic _.1) (dynamic _.2) (dynamic _.3) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3)))
    ((=== (cons '_.0 '_.1) (cons '_.2 '_.3))
     $$
     (absento (call _.0) (call _.1) (call _.2) (call _.3) (closure _.0)
              (closure _.1) (closure _.2) (closure _.3) (dynamic _.0)
              (dynamic _.1) (dynamic _.2) (dynamic _.3) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3)))
    ((=== '_.0 (cons '_.1 (cons '_.2 '_.3)))
     $$
     (absento (call _.0) (call _.1) (call _.2) (call _.3) (closure _.0)
              (closure _.1) (closure _.2) (closure _.3) (dynamic _.0)
              (dynamic _.1) (dynamic _.2) (dynamic _.3) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3)))
    ((=== '_.0 (cons (cons '_.1 '_.2) '_.3))
     $$
     (absento (call _.0) (call _.1) (call _.2) (call _.3) (closure _.0)
              (closure _.1) (closure _.2) (closure _.3) (dynamic _.0)
              (dynamic _.1) (dynamic _.2) (dynamic _.3) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3)))
    ((disj (=== '_.0 '_.1) (=== '_.2 '_.3))
     $$
     (absento (call _.0) (call _.1) (call _.2) (call _.3) (closure _.0)
              (closure _.1) (closure _.2) (closure _.3) (dynamic _.0)
              (dynamic _.1) (dynamic _.2) (dynamic _.3) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3)))
    ((=== (cons '_.0 (cons '_.1 '_.2)) '_.3)
     $$
     (absento (call _.0) (call _.1) (call _.2) (call _.3) (closure _.0)
              (closure _.1) (closure _.2) (closure _.3) (dynamic _.0)
              (dynamic _.1) (dynamic _.2) (dynamic _.3) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3)))
    ((=== '_.0 (cons (cons '_.1 '_.2) (cons '_.3 '_.4)))
     $$
     (absento (call _.0) (call _.1) (call _.2) (call _.3)
              (call _.4) (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (dynamic _.0) (dynamic _.1)
              (dynamic _.2) (dynamic _.3) (dynamic _.4) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3) (prim _.4)))
    ((call/fresh (lambda (_.0) (=== '_.1 '_.2)))
     $$
     (=/= ((_.0 call))
          ((_.0 closure))
          ((_.0 dynamic))
          ((_.0 prim)))
     (sym _.0)
     (absento (call _.1) (call _.2) (closure _.1) (closure _.2)
              (dynamic _.1) (dynamic _.2) (prim _.1) (prim _.2))))
 )

(test
    (run 10 (q)
      (evalo-unstaged
       (valid-ge? q)
       #t))
  '(((=== '_.0 '_.1)
   $$
   (absento (call _.0) (call _.1) (closure _.0) (closure _.1)
     (dynamic _.0) (dynamic _.1) (prim _.0) (prim _.1)))
  ((=== '_.0 (cons '_.1 '_.2))
    $$
    (absento (call _.0) (call _.1) (call _.2) (closure _.0)
      (closure _.1) (closure _.2) (dynamic _.0) (dynamic _.1)
      (dynamic _.2) (prim _.0) (prim _.1) (prim _.2)))
  ((=== (cons '_.0 '_.1) '_.2)
    $$
    (absento (call _.0) (call _.1) (call _.2) (closure _.0)
      (closure _.1) (closure _.2) (dynamic _.0) (dynamic _.1)
      (dynamic _.2) (prim _.0) (prim _.1) (prim _.2)))
  ((=== '_.0 (cons '_.1 (cons '_.2 '_.3)))
    $$
    (absento (call _.0) (call _.1) (call _.2) (call _.3) (closure _.0)
      (closure _.1) (closure _.2) (closure _.3) (dynamic _.0)
      (dynamic _.1) (dynamic _.2) (dynamic _.3) (prim _.0)
      (prim _.1) (prim _.2) (prim _.3)))
  ((=== (cons '_.0 '_.1) (cons '_.2 '_.3))
    $$
    (absento (call _.0) (call _.1) (call _.2) (call _.3) (closure _.0)
      (closure _.1) (closure _.2) (closure _.3) (dynamic _.0)
      (dynamic _.1) (dynamic _.2) (dynamic _.3) (prim _.0)
      (prim _.1) (prim _.2) (prim _.3)))
  ((conj (=== '_.0 '_.1) (=== '_.2 '_.3))
    $$
    (absento (call _.0) (call _.1) (call _.2) (call _.3) (closure _.0)
      (closure _.1) (closure _.2) (closure _.3) (dynamic _.0)
      (dynamic _.1) (dynamic _.2) (dynamic _.3) (prim _.0)
      (prim _.1) (prim _.2) (prim _.3)))
  ((=== '_.0 (cons (cons '_.1 '_.2) '_.3))
    $$
    (absento (call _.0) (call _.1) (call _.2) (call _.3) (closure _.0)
      (closure _.1) (closure _.2) (closure _.3) (dynamic _.0)
      (dynamic _.1) (dynamic _.2) (dynamic _.3) (prim _.0)
      (prim _.1) (prim _.2) (prim _.3)))
  ((call/fresh (lambda (_.0) (=== '_.1 '_.2)))
    $$
    (=/= ((_.0 call))
         ((_.0 closure))
         ((_.0 dynamic))
         ((_.0 prim)))
    (sym _.0)
    (absento (call _.1) (call _.2) (closure _.1) (closure _.2)
      (dynamic _.1) (dynamic _.2) (prim _.1) (prim _.2)))
  ((=== (cons '_.0 (cons '_.1 '_.2)) '_.3)
    $$
    (absento (call _.0) (call _.1) (call _.2) (call _.3) (closure _.0)
      (closure _.1) (closure _.2) (closure _.3) (dynamic _.0)
      (dynamic _.1) (dynamic _.2) (dynamic _.3) (prim _.0)
      (prim _.1) (prim _.2) (prim _.3)))
  ((disj (=== '_.0 '_.1) (=== '_.2 '_.3))
    $$
    (absento (call _.0) (call _.1) (call _.2) (call _.3) (closure _.0)
      (closure _.1) (closure _.2) (closure _.3) (dynamic _.0)
      (dynamic _.1) (dynamic _.2) (dynamic _.3) (prim _.0)
      (prim _.1) (prim _.2) (prim _.3)))))




;;; need gensym in the relational iterpreter in order to implement 'call/fresh'
;;; (in order to generate a new identifier for the new 'lambda')
#|
(define (micro-interp query)
  (micro
   `(letrec
        ((eval (lambda (ge)
                 (match ge
                   [`(=== ,t1 ,t2) (=== t1 t2)]
                   [`(conj ,ge1 ,ge2) (conj (eval ge1) (eval ge2))]
                   [`(disj ,ge1 ,ge2) (disj (eval ge1) (eval ge2))]
                   [`(call/fresh (lambda (q) ))]))))
      (eval ,query))))

(define-staged-relation (microo query)
  (micro-interp query))
|#
