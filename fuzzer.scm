;; n = z | s n
(define nato
  (eval
   (gen 'nat? '(x)
        '(if (equal? 'z x)
             #t
             (and (pair? x)
                  (pair? (cdr x))
                  (null? (cdr (cdr x)))
                  (equal? 's (car x))
                  (nat? (car (cdr x))))))))

(run 10 (q) (nato q #t))
(run 10 (q) (nato q #f))

;; l = ()| cons n l

(define listo
  (eval
   (gen 'list? '(x)
        '(if (null? x)
             #t
             (and (pair? x)
                  (number? (car x))
                  (list? (cdr x))
                  )))))

(run 10 (q) (listo q #t))
(run 10 (q) (listo q #f))


(define bin-treeo
(eval
(gen 'bin-tree? '(t)
     '(if (number? t)
	 #t
	 (and (pair? t)
	      (pair? (cdr t))
	      (pair? (cdr (cdr t)))
	      (pair? (cdr (cdr (cdr t))))
	      (null? (cdr (cdr (cdr (cdr t)))))
	      (equal? (car t) 'node)
	      (number? (car (cdr (cdr t))))
	      (bin-tree? (car (cdr t)))
	      (bin-tree? (car (cdr (cdr (cdr t))))))))
))

(run 10 (q) (bin-treeo q #t))

(run 10 (q) (bin-treeo q #f))
(time (length (run 10000 (q) (bin-treeo q #f))))
