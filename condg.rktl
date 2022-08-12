(define (evaluate-guard thunk-stream guard-stx)
  (match (take 2 thunk-stream)
    ['() #f]
    [(list answer) answer]
    [answers
     'nondet
     ;;(raise-syntax-error 'condg (format "guard produced too many answers: ~a" answers) guard-stx)
     ]))

(define (condg-runtime fallback clauses)
  (lambda (st)
    (let ((st (state-with-scope st (new-scope))))
      (define candidates
        (for/list ([clause clauses]
                   #:do [(match-define (list guard-stream body guard-stx) (clause st))
                         (define guard-answer (evaluate-guard (lambda () guard-stream) guard-stx))]
                   #:when guard-answer)
          (cons guard-answer body)))
      (match candidates
        ['() (error 'condg "no candidates")]
        [(list (cons guard-answer body))
         (if (eq? 'nondet guard-answer)
             (fallback st)
             (body guard-answer))]
        [_ (fallback st)]))))

(define-syntax condg
  (syntax-parser
   ((_ fallback ((x ...) (~and guard (g0 g ...)) (b0 b ...)) ...)
    #'(condg-runtime
       (lambda (st) (fallback st))
       (list
         (lambda (st)
           (let ((scope (subst-scope (state-S st))))
             (let ((x (var scope)) ...)
               (list
                 (bind* (g0 st) g ...)
                 (lambda (st) (bind* (b0 st) b ...))
                 #'guard))))
         ...)))))
