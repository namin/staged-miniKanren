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

(define nnf
  (lambda (concept)
    `(letrec ((positive?
               (lambda (n)
                 (match n
                   [`z #f]
                   [`(s . ,n-1) #t]))))
       (letrec ((add1
                 (lambda (n)
                   (cons 's n))))
         (letrec ((sub1
                   (lambda (n)
                     (match n
                       [`(s . ,n-1) n-1]))))
           (letrec ((nnf
                     (lambda (concept)
                       (match concept
                         [`(Not (Not ,c)) (nnf c)]
                         [`(Not (And ,c1 ,c2))
                          (list 'Or (nnf (list 'Not c1)) (nnf (list 'Not c2)))]
                         [`(Not (Or ,c1 ,c2))
                          (list 'And (nnf (list 'Not c1)) (nnf (list 'Not c2)))]
                         [`(Not (Exists ,r ,c))
                          (list 'All r (nnf (list 'Not c)))]
                         [`(Not (All ,r ,c))
                          (list 'Exists r (nnf (list 'Not c)))]
                         [`(Not (AtMost ,k ,r))
                          ;; TODO: enforce that r is a symbol
                          (list 'AtLeast (add1 k) r)]
                         [`(Not (AtLeast ,k ,r))
                          ;; TODO: enforce that r is a symbol
                          (if (positive? k)
                              (list 'AtMost (sub1 k) r)
                              (list 'Not 'Top))]                    
                         [`(And ,c1 ,c2)
                          (list 'And (nnf c1) (nnf c2))]
                         [`(Or ,c1 ,c2)
                          (list 'Or (nnf c1) (nnf c2))]
                         [`(Exists ,r ,c)
                          (list 'Exists r (nnf c))]
                         [`(All ,r ,c)
                          (list 'All r (nnf c))]
                         [concept concept]))))
             (nnf ',concept)))))))


(record-bench 'run-unstaged 'nnf-0)
(time-test
  (run 10 (concept)
    (evalo-unstaged
     (nnf concept)
     '(Not Top)))
  '((Not Top)
    (Not (Not (Not Top)))
    (Not (Not (Not (Not (Not Top)))))
    (Not (Not (Not (Not (Not (Not (Not Top)))))))
    (Not (Not (Not (Not (Not (Not (Not (Not (Not Top)))))))))
    (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not Top)))))))))))
    ((Not (AtLeast z _.0))
     $$
     (absento (call _.0) (closure _.0) (dynamic _.0) (prim _.0)))
    (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not Top)))))))))))))
    ((Not (Not (Not (AtLeast z _.0))))
     $$
     (absento (call _.0) (closure _.0) (dynamic _.0) (prim _.0)))
    (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not Top)))))))))))))))))


(record-bench 'run-unstaged 'nnf-0)
(time-test
  (run* (nnf-concept)
    (evalo-unstaged
     (nnf '(Not (AtLeast z hasChild)))
     nnf-concept))
  '((Not Top)))

;; why does the staged version fail, when the unstaged version succeeds?
(record-bench 'run-staged 'nnf-0)
(time-test
  (run-staged #f (nnf-concept)
    (evalo-staged
     (nnf '(Not (AtLeast z hasChild)))
     nnf-concept))
  '((Not Top)))

(record-bench 'run-staged 'nnf-0)
(time-test
  (run #f (nnf-concept)
    (evalo-staged
     (nnf '(Not (AtLeast z hasChild)))
     nnf-concept))
  '((Not Top)))



(record-bench 'run-unstaged 'nnf-0)
(time-test
  (run* (nnf-concept)
    (evalo-unstaged
     (nnf '(Not (AtMost (s . z) hasChild)))
     nnf-concept))
  '((AtLeast (s s . z) hasChild)))



(record-bench 'run-unstaged 'nnf-0)
(time-test
  (run* (nnf-concept)
    (evalo-unstaged
     (nnf '(Not (AtLeast (s s s . z) hasChild)))
     nnf-concept))
  '((AtMost (s s . z) hasChild)))

;; this fails, even though the unstaged version succeeds!
(record-bench 'run-staged 'nnf-0)
(time-test
  (run-staged #f (nnf-concept)
    (evalo-staged
     (nnf '(Not (AtLeast (s s s . z) hasChild))) nnf-concept))
  '((AtMost (s s . z) hasChild)))



(record-bench 'run-unstaged 'nnf-0)
(time-test
  (run* (nnf-concept)
    (evalo-unstaged
     `(letrec ((positive?
                  (lambda (n)
                    (match n
                      [`z #f]
                      [`(s . ,n-1) #t]))))
          (letrec ((add1
                    (lambda (n)
                      (cons 's n))))
            (letrec ((sub1
                      (lambda (n)
                        (match n
                          [`(s . ,n-1) n-1]))))
              (letrec ((nnf
                        (lambda (concept)
                          (match concept
                            [`(Not (Not ,c)) (nnf c)]
                            [`(Not (And ,c1 ,c2))
                             (list 'Or (nnf (list 'Not c1)) (nnf (list 'Not c2)))]
                            [`(Not (Or ,c1 ,c2))
                             (list 'And (nnf (list 'Not c1)) (nnf (list 'Not c2)))]
                            [`(Not (Exists ,r ,c))
                             (list 'All r (nnf (list 'Not c)))]
                            [`(Not (All ,r ,c))
                             (list 'Exists r (nnf (list 'Not c)))]                          
                            [`(Not (AtMost ,k ,r))
                             (list 'AtLeast (add1 k) r)]                          
                            [`(Not (AtLeast ,k ,r))
                             (if (positive? k)
                                 (list 'AtMost (sub1 k) r)
                                 (list 'Not 'Top))]                    
                            [`(And ,c1 ,c2)
                             (list 'And (nnf c1) (nnf c2))]
                            [`(Or ,c1 ,c2)
                             (list 'Or (nnf c1) (nnf c2))]
                            [`(Exists ,r ,c)
                             (list 'Exists r (nnf c))]
                            [`(All ,r ,c)
                             (list 'All r (nnf c))]
                            [concept concept]))))
                (nnf '(Not (AtLeast (s s s . z) hasChild)))))))
     nnf-concept))
  '((AtMost (s s . z) hasChild)))


(record-bench 'run-unstaged 'nnf-0)
(time-test
  (run* (nnf-concept)
    (evalo-unstaged
     `(letrec ((nnf
                (lambda (concept)
                  (match concept
                    [`(Not (Not ,c)) (nnf c)]
                    [`(Not (And ,c1 ,c2))
                     (list 'Or (nnf (list 'Not c1)) (nnf (list 'Not c2)))]
                    [`(Not (Or ,c1 ,c2))
                     (list 'And (nnf (list 'Not c1)) (nnf (list 'Not c2)))]
                    [`(Not (Exists ,r ,c))
                     (list 'All r (nnf (list 'Not c)))]
                    [`(Not (All ,r ,c))
                     (list 'Exists r (nnf (list 'Not c)))]
                    #;[`(Not (AtMost ,k ,r)) `(AtLeast ,(+ k 1) ,r)]
                    #;[`(Not (AtLeast ,k ,r))
                    (if (> k 0)          ; ;
                    `(AtMost ,(- k 1) ,r) ; ;
                    `(Not Top))]
                    [`(And ,c1 ,c2)
                     (list 'And (nnf c1) (nnf c2))]
                    [`(Or ,c1 ,c2)
                     (list 'Or (nnf c1) (nnf c2))]
                    [`(Exists ,r ,c)
                     (list 'Exists r (nnf c))]
                    [`(All ,r ,c)
                     (list 'All r (nnf c))]
                    [concept concept]))))
        (nnf '(Not (And (Atomic C) (Atomic D)))))
     nnf-concept))
  '((Or (Not (Atomic C)) (Not (Atomic D)))))



(record-bench 'run-unstaged 'nnf-1)
(time-test
  (run 10 (concept)
    (evalo-unstaged
     `(letrec ((nnf
                (lambda (concept)
                  (match concept
                    [`(Not (Not ,c)) (nnf c)]
                    [`(Not (And ,c1 ,c2))
                     (list 'Or (nnf (list 'Not c1)) (nnf (list 'Not c2)))]
                    #;[`(Not (Or ,c1 ,c2)) `(And ,(nnf `(Not ,c1)) ,(nnf `(Not ,c2)))]
                    #;[`(Not (Exists ,r ,c)) `(All ,r ,(nnf `(Not ,c)))]
                    #;[`(Not (All ,r ,c)) `(Exists ,r ,(nnf `(Not ,c)))]
                    #;[`(Not (AtMost ,k ,r)) `(AtLeast ,(+ k 1) ,r)]
                    #;[`(Not (AtLeast ,k ,r))
                    (if (> k 0)          ; ;
                    `(AtMost ,(- k 1) ,r) ; ;
                    `(Not Top))]
                    #;[`(And ,c1 ,c2) `(And ,(nnf c1) ,(nnf c2))]
                    #;[`(Or ,c1 ,c2) `(Or ,(nnf c1) ,(nnf c2))]
                    #;[`(Exists ,r ,c) `(Exists ,r ,(nnf c))]
                    #;[`(All ,r ,c) `(All ,r ,(nnf c))]
                    [concept concept]))))
        (nnf ',concept))
     'Top))
  '(Top
    (Not (Not Top))
    (Not (Not (Not (Not Top))))
    (Not (Not (Not (Not (Not (Not Top))))))
    (Not (Not (Not (Not (Not (Not (Not (Not Top))))))))
    (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not Top))))))))))
    (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not Top))))))))))))
    (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not Top))))))))))))))
    (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not Top))))))))))))))))
    (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not Top))))))))))))))))))))

(record-bench 'run-staged 'nnf-1)
(time-test
  (run-staged 10 (concept)
    (evalo-staged
     `(letrec ((nnf
                (lambda (concept)
                  (match concept
                    [`(Not (Not ,c)) (nnf c)]
                    [`(Not (And ,c1 ,c2))
                     (list 'Or (nnf (list 'Not c1)) (nnf (list 'Not c2)))]
                    #;[`(Not (Or ,c1 ,c2)) `(And ,(nnf `(Not ,c1)) ,(nnf `(Not ,c2)))]
                    #;[`(Not (Exists ,r ,c)) `(All ,r ,(nnf `(Not ,c)))]
                    #;[`(Not (All ,r ,c)) `(Exists ,r ,(nnf `(Not ,c)))]
                    #;[`(Not (AtMost ,k ,r)) `(AtLeast ,(+ k 1) ,r)]
                    #;[`(Not (AtLeast ,k ,r))
                    (if (> k 0)          ; ;
                    `(AtMost ,(- k 1) ,r) ; ;
                    `(Not Top))]
                    #;[`(And ,c1 ,c2) `(And ,(nnf c1) ,(nnf c2))]
                    #;[`(Or ,c1 ,c2) `(Or ,(nnf c1) ,(nnf c2))]
                    #;[`(Exists ,r ,c) `(Exists ,r ,(nnf c))]
                    #;[`(All ,r ,c) `(All ,r ,(nnf c))]
                    [concept concept]))))
        (nnf ',concept))
     'Top))
  '(Top
    (Not (Not Top))
    (Not (Not (Not (Not Top))))
    (Not (Not (Not (Not (Not (Not Top))))))
    (Not (Not (Not (Not (Not (Not (Not (Not Top))))))))
    (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not Top))))))))))
    (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not Top))))))))))))
    (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not Top))))))))))))))
    (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not Top))))))))))))))))
    (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not (Not Top))))))))))))))))))))




(record-bench 'run-unstaged 'nnf-1)
(time-test
  (run* (nnf-concept)
    (evalo-unstaged
     `(letrec ((nnf
                (lambda (concept)
                  (match concept
                    [`(Not (Not ,c)) (nnf c)]
                    [`(Not (And ,c1 ,c2))
                     (list 'Or (nnf (list 'Not c1)) (nnf (list 'Not c2)))]
                    #;[`(Not (Or ,c1 ,c2)) `(And ,(nnf `(Not ,c1)) ,(nnf `(Not ,c2)))]
                    #;[`(Not (Exists ,r ,c)) `(All ,r ,(nnf `(Not ,c)))]
                    #;[`(Not (All ,r ,c)) `(Exists ,r ,(nnf `(Not ,c)))]
                    #;[`(Not (AtMost ,k ,r)) `(AtLeast ,(+ k 1) ,r)]
                    #;[`(Not (AtLeast ,k ,r))
                    (if (> k 0)          ; ;
                    `(AtMost ,(- k 1) ,r) ; ;
                    `(Not Top))]
                    #;[`(And ,c1 ,c2) `(And ,(nnf c1) ,(nnf c2))]
                    #;[`(Or ,c1 ,c2) `(Or ,(nnf c1) ,(nnf c2))]
                    #;[`(Exists ,r ,c) `(Exists ,r ,(nnf c))]
                    #;[`(All ,r ,c) `(All ,r ,(nnf c))]
                    [concept concept]))))
        (nnf 'Top))
     nnf-concept))
  '(Top))

(record-bench 'run-staged 'nnf-1)
(time-test
  (run-staged #f (nnf-concept)
    (evalo-staged
     `(letrec ((nnf
                (lambda (concept)
                  (match concept
                    [`(Not (Not ,c)) (nnf c)]
                    [`(Not (And ,c1 ,c2))
                     (list 'Or (nnf (list 'Not c1)) (nnf (list 'Not c2)))]
                    #;[`(Not (Or ,c1 ,c2)) `(And ,(nnf `(Not ,c1)) ,(nnf `(Not ,c2)))]
                    #;[`(Not (Exists ,r ,c)) `(All ,r ,(nnf `(Not ,c)))]
                    #;[`(Not (All ,r ,c)) `(Exists ,r ,(nnf `(Not ,c)))]
                    #;[`(Not (AtMost ,k ,r)) `(AtLeast ,(+ k 1) ,r)]
                    #;[`(Not (AtLeast ,k ,r))
                    (if (> k 0)          ; ;
                    `(AtMost ,(- k 1) ,r) ; ;
                    `(Not Top))]
                    #;[`(And ,c1 ,c2) `(And ,(nnf c1) ,(nnf c2))]
                    #;[`(Or ,c1 ,c2) `(Or ,(nnf c1) ,(nnf c2))]
                    #;[`(Exists ,r ,c) `(Exists ,r ,(nnf c))]
                    #;[`(All ,r ,c) `(All ,r ,(nnf c))]
                    [concept concept]))))
        (nnf 'Top))
     nnf-concept))
  '(Top))


#!eof

(define 
  (lambda (concept)
    (match concept
      [`(Not (Not ,c)) (nnf c)]
      [`(Not (And ,c1 ,c2))
       (list 'Or (nnf (list 'Not c1)) (nnf (list 'Not c2)))]
      #;[`(Not (Or ,c1 ,c2)) `(And ,(nnf `(Not ,c1)) ,(nnf `(Not ,c2)))]
      #;[`(Not (Exists ,r ,c)) `(All ,r ,(nnf `(Not ,c)))]
      #;[`(Not (All ,r ,c)) `(Exists ,r ,(nnf `(Not ,c)))]
      #;[`(Not (AtMost ,k ,r)) `(AtLeast ,(+ k 1) ,r)]
      #;[`(Not (AtLeast ,k ,r))
       (if (> k 0)
           `(AtMost ,(- k 1) ,r)
           `(Not Top))]
      #;[`(And ,c1 ,c2) `(And ,(nnf c1) ,(nnf c2))]
      #;[`(Or ,c1 ,c2) `(Or ,(nnf c1) ,(nnf c2))]
      #;[`(Exists ,r ,c) `(Exists ,r ,(nnf c))]
      #;[`(All ,r ,c) `(All ,r ,(nnf c))]
      [concept concept])))


#!eof

(define nnf
  (lambda (concept)
    (match concept
      [`(Not (Not ,c)) (nnf c)]
      [`(Not (And ,c1 ,c2)) `(Or ,(nnf `(Not ,c1)) ,(nnf `(Not ,c2)))]
                            ;; == (list 'Or (nnf (list 'Not c1)) (nnf (list 'Not c2))))
      [`(Not (Or ,c1 ,c2)) `(And ,(nnf `(Not ,c1)) ,(nnf `(Not ,c2)))]
      [`(Not (Exists ,r ,c)) `(All ,r ,(nnf `(Not ,c)))]
      [`(Not (All ,r ,c)) `(Exists ,r ,(nnf `(Not ,c)))]
      [`(Not (AtMost ,k ,r)) `(AtLeast ,(+ k 1) ,r)]
      [`(Not (AtLeast ,k ,r))
       (if (> k 0)
           `(AtMost ,(- k 1) ,r)
           `(Not Top))]
      [`(And ,c1 ,c2) `(And ,(nnf c1) ,(nnf c2))]
      [`(Or ,c1 ,c2) `(Or ,(nnf c1) ,(nnf c2))]
      [`(Exists ,r ,c) `(Exists ,r ,(nnf c))]
      [`(All ,r ,c) `(All ,r ,(nnf c))]
      [concept concept])))
