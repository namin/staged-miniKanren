(define (map-on-syntax-data f stx)
  (let rec ((v stx))
    (cond
      ((syntax? v)
       (datum->syntax v (rec (syntax-e v)) v v))
      ((pair? v)
       (cons (rec (car v)) (rec (cdr v))))
      ((data? v)
       (f (data-value v)))
      (else v))))

(define (reify-S-syntax v S)
  (cond
    ((syntax? v)
     (reify-S-syntax (syntax-e v) S))
    ((pair? v)
     (let ((S (reify-S-syntax (car v) S)))
       (reify-S-syntax (cdr v) S)))
    ((data? v)
     (reify-S (data-value v) S))
    (else S)))

(define (do-expand t)
  (map-on-syntax-data reflect-datum t))

(define (unexpand x) x)
(define (unexpand? x) (syntax? x))

(struct data [value] #:transparent)
(define (expand x) (data x))
(define (expand? x) (data? x))

(define (reflect-datum t)
  (define (nonliteral-datum? t)
    (or (var? t) (syntax? t) (apply-rep? t)))
  
  (define (needs-unquote? t)
    (cond
      ((nonliteral-datum? t) #t)
      ((pair? t) (or (needs-unquote? (car t)) (needs-unquote? (cdr t))))
      (else #f)))

  (define (reflect-nonliteral-datum t)
    (match t
      [(? var?) (data t)]
      [(? syntax? t) (do-expand t)]
      [(apply-rep name-staged name-dyn args proc)
       #`(make-apply-rep
          #,(reflect-datum name-staged)
          #,(reflect-datum name-dyn)
          #,(reflect-datum args)
          #,(reflect-datum proc))]))
  
  (define (reflect-quasi-contents t)
    (match t
      [(? nonliteral-datum?) #`,#,(reflect-nonliteral-datum t)]
      [(cons a d)
       (cons (reflect-quasi-contents a) (reflect-quasi-contents d))]
      [t t]))

  (cond
    [(nonliteral-datum? t)
     (reflect-nonliteral-datum t)]
    [(needs-unquote? t)
     #``#,(reflect-quasi-contents t)]
    [else
     #`'#,(reflect-quasi-contents t)]))

(define (walk*-syntax stx S)
  (map-on-syntax-data (lambda (v) (data (walk* v S))) stx))

(define walk-later
  (lambda (L S)
    (map (lambda (stx) (walk* stx S)) (reverse L))))

(define walk-later-final
  (lambda (L S)
    (map do-expand (walk-later L S))))

(define later
  (lambda (x)
    (lambda (st)
      (state (state-S st) (state-C st) (cons x (state-L st))))))

(define later-scope
  (lambda (g out)
    (lambda (st)
      (bind*
       (g (state (state-S st) (C-new-later-scope (state-C st)) '()))
       generate-constraints
       (lambda (st2)
         ((fresh ()
            (== out (walk-later (state-L st2) (state-S st2))))
          st))))))

(define capture-later
  (lambda (g k)
    (lambda (st)
      (bind*
       (g (state-with-scope
           (state (state-S st) (C-new-later-scope (state-C st)) '())
           (new-scope)))
       generate-constraints
       (lambda (st2)
         ((k (walk-later (state-L st2) (state-S st2)))
          st))))))

(define (ldisj . gs-init)
  (let recur ([gs gs-init] [gs-stx '()])
    (if (null? gs)
        (later #`(conde #,@(reverse gs-stx)))
        (capture-later (car gs)
                       (lambda (g-stx)
                         (recur (cdr gs) (cons g-stx gs-stx)))))))

(define-syntax lconde
  (syntax-rules ()
    ((_ (g ...) ...)
     (ldisj (fresh () g ...) ...))))

;; TODO: this relies on internal details, only works for current set of type constraints.
;;  Should figure how to make generic in type constraints at least.
(define (generate-var-constraints st)
  (lambda (v)
    (let ([c (lookup-c st v)])
      (if (eq? c empty-c)
          st
          (append
           (if (c-T c)
               (let ((cid (hash-ref 
                           (hasheq 'sym #'symbolo 'num #'numbero 'str #'stringo)
                           (type-constraint-reified (c-T c)))))
                 (list #`(#,cid #,(expand v))))
               '())
           (map (lambda (atom) #`(absento #,(expand atom) #,v)) (c-A c))
           (map (lambda (d) #`(=/=* #,(expand d))) (c-D c)))))))

(define generate-constraints
  (lambda (st)
    (let* ([vars (remove-duplicates (C-vars (state-C st)))]
           [new-stx (apply append (map (generate-var-constraints st) vars))])
      (state (state-S st) (state-C st) (append (state-L st) (reverse new-stx))))))

(define (later-binary-constraint constraint-id)
  (lambda (t1 t2) (later #`(#,constraint-id #,(expand t1) #,(expand t2)))))

(define (later-unary-constraint constraint-id)
  (lambda (t) (later #`(#,constraint-id #,(expand t)))))

(define-values (l== l=/= labsento)
  (apply values (map later-binary-constraint (list #'== #'=/= #'absento))))

(define-values (lsymbolo lnumbero lstringo)
  (apply values (map later-unary-constraint (list #'symbolo #'numbero #'stringo))))

(define-syntax lapp
  (syntax-rules ()
    [(_ relation arg ...)
     (later #`(relation #,(expand arg) ...))]))

(define lfail (later #'fail))
(define lsucceed (later #'succeed))
