(letrec ((zero?
          (lambda (n)
            (equal? 'z n)))
         (add1
          (lambda (n)
            (cons 's n)))
         (sub1
          (lambda (n)
            (and (equal? (car n) 's)
                 (cdr n))))
         (=
          (lambda (n m)
            (if (and (zero? n) (zero? m))
                #t
                (if (zero? n)
                    #f
                    (if (zero? m)
                        #f
                        (= (sub1 n) (sub1 m)))))))
         (+
          (lambda (n m)
            (if (zero? n)
                m
                (add1 (+ (sub1 n) m)))))
         (-
          (lambda (n m)
            (if (zero? m)
                n
                (sub1 (- n (sub1 m))))))
         (*
          (lambda (n m)
            (if (zero? n)
                (zero)
                (+ (* (sub1 n) m) m))))
         (zero
          (lambda ()
            'z))
         (one
          (lambda ()
            (add1 (zero))))
         (two
          (lambda ()
            (add1 (add1 (zero)))))
         (!
          (lambda (n)
            (if (zero? n)
                (one)
                (* n (! (sub1 n))))))
         (!-aps
          (lambda (n a)
            (if (zero? n)
                a
                (!-aps (sub1 n) (* n a)))))
         (fib-aps
          (lambda (n a1 a2)
            (if (zero? n)
                a1
                (if (zero? (sub1 n))
                    a2
                    (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))))
  (fib-aps '(s s s s s s . z) 'z '(s . z)))
;; => (s s s s s s s s . z)


(letrec ((zero?
          (lambda (n)
            (equal? 'z n))))
  (letrec ((add1
            (lambda (n)
              (cons 's n))))
    (letrec ((sub1
              (lambda (n)
                (and (equal? (car n) 's)
                     (cdr n)))))
      (letrec ((=
                (lambda (n m)
                  (if (and (zero? n) (zero? m))
                      #t
                      (if (zero? n)
                          #f
                          (if (zero? m)
                              #f
                              (= (sub1 n) (sub1 m))))))))
        (letrec ((+
                  (lambda (n m)
                    (if (zero? n)
                        m
                        (add1 (+ (sub1 n) m))))))
          (letrec ((-
                    (lambda (n m)
                      (if (zero? m)
                          n
                          (sub1 (- n (sub1 m)))))))
            (letrec ((*
                      (lambda (n m)
                        (if (zero? n)
                            (zero)
                            (+ (* (sub1 n) m) m)))))
              (letrec ((zero
                        (lambda ()
                          'z)))
                (letrec ((one
                          (lambda ()
                            (add1 (zero)))))
                  (letrec ((two
                            (lambda ()
                              (add1 (add1 (zero))))))
                    (letrec ((!
                              (lambda (n)
                                (if (zero? n)
                                    (one)
                                    (* n (! (sub1 n)))))))
                      (letrec ((!-aps
                                (lambda (n a)
                                  (if (zero? n)
                                      a
                                      (!-aps (sub1 n) (* n a))))))
                        (letrec ((fib-aps
                                  (lambda (n a1 a2)
                                    (if (zero? n)
                                        a1
                                        (if (zero? (sub1 n))
                                            a2
                                            (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))))
                          (fib-aps '(s s s s s s . z) 'z '(s . z)))))))))))))))
;; => (s s s s s s s s . z)
