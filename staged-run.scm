(define (evalo-staged expr val)
  (eval-expo #t expr initial-env val))

(define-syntax run-staged
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (printf "running first stage\n")
     (let* ((f (gen-func (run 100 (q) g0 g ...)))
            (e (eval f)))
       (printf "running second stage\n")
       (run n (q) (e q))))))
