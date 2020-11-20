;; using namin/faster-miniKaren branch staged
(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")
(load "staged-interp.scm")
(load "staged-utils.scm")

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

;; l = '() | cons n l

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

(define bin-search-treeo
(eval
(gen 'bin-search-tree? '(t)
     '(if (number? t)
      #t
      (and (pair? t)
           (pair? (cdr t))
           (pair? (cdr (cdr t)))
           (pair? (cdr (cdr (cdr t))))
           (null? (cdr (cdr (cdr (cdr t)))))
           (equal? (car t) 'node)
           (number? (car (cdr (cdr t))))
	   (> (car (cdr (cdr t)))
	      (car (cdr (cdr (car (cdr t))))))
           (< (car (cdr (cdr t)))
	      (car (cdr (cdr  (car (cdr (cdr (cdr t))))))))
           (bin-search-tree? (car (cdr t)))
           (bin-search-tree? (car (cdr (cdr (cdr t))))))))
))

(define quasi-listo
  (eval
   (gen 'list? '(x)
        '(if (null? x)
             #t
             (and (pair? x)
                  (number? (car x))
                  (choice (number? (cdr x)) (list? (cdr x)))
                  )))))

(test (run 10 (q) (quasi-listo q #t))
      '((() !! ())
	(((_.0 . _.1) !! ()) (num _.0 _.1))
	(((_.0) !! ()) (num _.0))
	(((_.0 _.1 . _.2) !! ()) (num _.0 _.1 _.2))
	(((_.0 _.1) !! ()) (num _.0 _.1))
	(((_.0 _.1 _.2 . _.3) !! ()) (num _.0 _.1 _.2 _.3))
	(((_.0 _.1 _.2) !! ()) (num _.0 _.1 _.2))
	(((_.0 _.1 _.2 _.3 . _.4) !! ()) (num _.0 _.1 _.2 _.3 _.4))
	(((_.0 _.1 _.2 _.3) !! ()) (num _.0 _.1 _.2 _.3))
	(((_.0 _.1 _.2 _.3 _.4 . _.5) !! ()) (num _.0 _.1 _.2 _.3 _.4 _.5))))

(test (run 10 (q) (quasi-listo q #f))
      '(((_.0 !! ()) (sym _.0)) ((_.0 !! ()) (num _.0)) ((closure . _.0) !! ())
       (((_.0 . _.1) !! ()) (=/= ((_.0 closure))) (sym _.0))
       (((_.0 . _.1) !! ()) (num _.0) (sym _.1))
       (((_.0 . _.1) . _.2) !! ())
       (((_.0 _.1 . _.2) !! ()) (num _.0))
       (((_.0 . _.1) !! ()) (num _.0) (sym _.1))
       (((_.0 . _.1) !! ()) (num _.0 _.1))
       (((_.0 closure . _.1) !! ()) (num _.0))))
