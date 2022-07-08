(define (evaluate-guard goal-state)
  (match (take #f goal-state)
    ['() #f]
    [(list answer) answer]
    [answers (error 'condg "guard produced too many answers" answers)]))

(define (condg-runtime fallback clauses)
  (lambda (st)
    (let ((st (state-with-scope st (new-scope))))
      (define candidates
        (for/fold ([candidates '()]) ([clause clauses])
          (match clause
            [(cons guard body)
             (let ((guard-answer (evaluate-guard (lambda () (guard st)))))
               (if guard-answer
                   (cons (cons guard-answer body) candidates)
                   candidates))])))
      (match candidates
        ['() (error 'condg "no candidates")]
        [(list (cons guard-answer body))
         (body guard-answer)]
        [_ (fallback st)]))))

(define-syntax condg
  (syntax-rules ()
    ((_ fallback (() (g0 g ...) (b0 b ...)) ...)
     (condg-runtime
      (lambda (st) (fallback st))
      (list
       (cons
        (lambda (st) (bind* (g0 st) g ...))
        (lambda (st) (bind* (b0 st) b ...))) ...)))))
