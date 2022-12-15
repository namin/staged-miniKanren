;; TODO: constraints like absento and probably reification need to know about apply-rep.
;; TODO: need a story for letrec

(define-syntax lreify-call
  (lambda (stx)
    (syntax-case stx ()
        ((_ rep ((rel-staged rel-dyn) (x ...) (y ...)))
         (andmap (lambda (id) (free-identifier=? id #'_)) (syntax->list #'(y ...)))
         (with-syntax
          (((y-n ...) (generate-temporaries #'(y ...)))
           ((y-n2 ...) (generate-temporaries #'(y ...))))
          #'(fresh (y-n ...)
              (capture-later
               (fresh ()
                 (l== y-n (unexpand #'y-n2)) ...
                 (rel-staged rep x ... y-n ...))
               (lambda (body)
                 (l== rep (make-apply-rep
                           'rel-staged 'rel-dyn (list x ...)
                           (unexpand #`(lambda (y-n2 ...)
                                        (fresh ()
                                          . #,body)))))))))))))

(define-syntax reify-call
  (lambda (stx)
    (syntax-case stx ()
        ((_ rep ((rel-staged rel-dyn) (x ...) (y ...)))
         (andmap (lambda (id) (free-identifier=? id #'_)) (syntax->list #'(y ...)))
         #'(== rep (make-apply-rep
                    'rel-staged 'rel-dyn (list x ...)
                    #f))))))

(define-syntax apply-reified
  (lambda (stx)
    (syntax-case stx ()
        ((_ rep ((rel-staged rel-dyn) (x ...) (y ...)))
         (andmap (lambda (id) (free-identifier=? id #'_)) (syntax->list #'(x ...)))
         (with-syntax
          (((x-n ...) (generate-temporaries #'(x ...))))
          #'(lambda (st)
              (let ((rep (walk rep (state-S st))))
                        ;;(printf "project~\n")
                        ((cond
                           ((var? rep)
                            (fresh (x-n ...)
                                   (== rep (make-apply-rep
                                             'rel-staged 'rel-dyn (list x-n ...) ;
                                             #f))
                                   (rel-dyn x-n ... y ...)))
                           ((apply-rep? rep)
                            ;;(printf "applying rep...~\n")
                            (let ((proc (apply-rep-proc rep)))
                              ;; TODO: unify to check names
                              (if (or (not proc) (unexpand? proc))
                                (apply rel-dyn (append (apply-rep-args rep) (list y ...)))
                                (begin
                                  ;;(printf "calling proc...~\n")
                                  (proc y ...)))))
                           (else fail)) st))))))))

(define-syntax lapply-reified
  (syntax-rules ()
    ((_ rep ((rel-staged rel-dyn) (x ...) (y ...)))
     (later `(apply-reified ,rep ((rel-staged rel-dyn) (x ...) (,(expand y) ...)))))))
