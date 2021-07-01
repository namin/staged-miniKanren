;; using namin/faster-miniKaren branch staged -- essential for this example
(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(load "staged-interp.scm")
(load "staged-utils.scm")
(load "staged-run.scm")

(load "unstaged-interp.scm")

(load "test-check.scm")


(define (micro query)
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

(test
    (run-staged 1 (q v)
      (evalo-staged
       (micro '((=== 5 5) (empty-state)))
       v))
  '((_.0 ((() . z)))))


(test
    (run-staged 1 (q v)
      (evalo-staged
       (micro '((call/fresh (lambda (q) (=== q 5))) (empty-state)))
       v))
  '((_.0 (((((var . z) . 5)) s . z)))))

#|
This generates answers that are not valid microKanren programs.
(run-staged 3 (q v)
  (evalo-staged
   (micro `(,q (empty-state)))
   '((() . z))))
|#
