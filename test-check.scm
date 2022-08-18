(define (tree-contains tree atom)
  (cond
    ((null? tree) #f)
    ((pair? tree)
     (or (tree-contains (car tree) atom)
         (tree-contains (cdr tree) atom)))
    (else (equal? tree atom))))


(define (record-bench phase name . args)
  (when res
    (format #t "generated code contains u-eval-expo: ~a~%"
            (tree-contains res 'u-eval-expo)))
  (if (null? args)
      (printf "BENCH ~a ~a\n" phase name)
      (printf "BENCH ~a ~a ~a\n" phase name (car args)))
  (set! res #f))

(define test-failed #f)
(define (set-test-failed!)
  (set! test-failed #t))

(define-syntax test
  (syntax-rules ()
    ((_ tested-expression expected-result)
     (begin
       (printf "Testing ~a\n" 'tested-expression)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (begin
               (set-test-failed!)
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
