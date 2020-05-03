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
	      (bin-tree? (car (cdr (cdr (cdr t)))))
	      )))

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
