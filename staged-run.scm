(define (evalo-staged expr val)
  (eval-expo #t expr initial-env val))

(define (evalo-unstaged expr val)
  (u-eval-expo expr initial-env val))

(define-syntax run-staged
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (begin
       (printf "running first stage\n")
       (let* ((f (gen-func
                  (parameterize ((staging-time? #t))
                    (run 100 (q) g0 g ...))))
              (e (eval f)))
         (printf "running second stage\n")
         (run n (q) (e q)))))
    ((_ n (q0 q1 q ...) g0 g ...)
     (run-staged n (x)
       (fresh (q0 q1 q ...)
         g0 g ...
         (l== `(,q0 ,q1 ,q ...) x))))))

(define-syntax run-staged*
  (syntax-rules ()
    ((_ (q0 q ...) g0 g ...) (run-staged #f (q0 q ...) g0 g ...))))

(define-syntax define-relation
  (syntax-rules ()
    ((_ (name x ...) g0 g ...)
     (define (name x ...)
       (fresh () g0 g ...)))))

(define-syntax define-staged-relation
  (syntax-rules ()
    ((_ (name x0 x ...) g0 g ...)
     (define name
       (eval
        (time
         (gen-func-rel
          (parameterize ((staging-time? #t))
            (run 100 (x0 x ...) g0 g ...))
          'x0 'x ...)))))))
