(define (evaluate-guard goal-state)
  (let ((res (take #f goal-state)))
    (cond
      ((null? res) #f)
      ((null? (cdr res)) (car res))
      (else (error 'condg "guard produced too many answers" res)))))

(define (condg-runtime fallback clauses)
  (lambda (st)
    (let ((st (state-with-scope st (new-scope))))
      (let loop ((clauses clauses) (candidates '()))
        (if (null? clauses)
            (cond ((null? candidates)
                   (error 'condg "no candidates"))
                  ((null? (cdr candidates))
                   (let ((guard-answer (car (car candidates)))
                         (body (cdr (car candidates))))
                     (body guard-answer)))
                  (else (fallback st)))
            (let ((guard (car (car clauses)))
                  (body (cdr (car clauses))))
              (let ((guard-answer (evaluate-guard (lambda () (guard st)))))
                (if guard-answer
                    (loop (cdr clauses) (cons (cons guard-answer body) candidates))
                    (loop (cdr clauses) candidates)))))))))

(define-syntax condg
  (syntax-rules ()
    ((_ fallback (() (g0 g ...) (b0 b ...)) ...)
     (condg-runtime
      (lambda (st) (fallback st))
      (list
       (cons
        (lambda (st) (bind* (g0 st) g ...))
        (lambda (st) (bind* (b0 st) b ...))) ...)))))
