(define (evaluate-guard thunk-stream)
  (match (take #f thunk-stream)
    ['() #f]
    [(list answer) answer]
    [answers (error 'condg "guard produced too many answers" answers)]))

(define (condg-runtime fallback clauses)
  (lambda (st)
    (let ((st (state-with-scope st (new-scope))))
      (define candidates
        (for/list ([clause clauses]
                   #:do [(match-define (cons guard-stream body) (clause st))
                         (define guard-answer (evaluate-guard (lambda () guard-stream)))]
                   #:when guard-answer)
          (cons guard-answer body)))
      (match candidates
        ['() (error 'condg "no candidates")]
        [(list (cons guard-answer body))
         (body guard-answer)]
        [_ (fallback st)]))))

(define-syntax condg
  (syntax-rules ()
    ((_ fallback ((x ...) (g0 g ...) (b0 b ...)) ...)
     (condg-runtime
      (lambda (st) (fallback st))
      (list
       (lambda (st)
         (let ((scope (subst-scope (state-S st))))
           (let ((x (var scope)) ...)
             (cons
              (bind* (g0 st) g ...)
              (lambda (st) (bind* (b0 st) b ...))))))
       ...)))))
