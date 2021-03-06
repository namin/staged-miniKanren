(define test-failed #f)

(define-syntax test
  (syntax-rules ()
    ((_ tested-expression expected-result)
     (begin
       (printf "Testing ~a\n" 'tested-expression)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (begin
               (set! test-failed #t)
               (error 'test
                      (format "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                              'tested-expression expected produced))
               ;; (format #t "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               ;;         'tested-expression expected produced)
               )))))))

(define-syntax time-test
  (syntax-rules ()
    ((_ tested-expression expected-result)
     (test
       (time tested-expression)
       expected-result))))
     
(define-syntax todo
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (printf "TODO ~s\n" title))))
