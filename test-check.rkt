#lang racket/base

(provide record-bench
         test
         time-test
         todo
         *test-result-same?*)

(require "private/internals.rkt"
         (for-syntax racket/base syntax/parse))

(define (tree-contains tree atom)
  (cond
    ((null? tree) #f)
    ((pair? tree)
     (or (tree-contains (car tree) atom)
         (tree-contains (cdr tree) atom)))
    (else (equal? tree atom))))

(define (tree-count tree atom)
  (cond
    ((null? tree) 0)
    ((pair? tree)
     (+ (tree-count(car tree) atom)
        (tree-count (cdr tree) atom)))
    ((equal? tree atom) 1)
    (else 0)))


(define (record-bench phase name . args)
  (when (generated-code)
    (printf "generated code u-eval-expo count: ~a~%"
            (tree-count (generated-code) 'invoke-fallback)))
  (if (null? args)
      (printf "BENCH ~a ~a\n" phase name)
      (printf "BENCH ~a ~a ~a\n" phase name (car args)))
  (reset-generated-code!))

(define test-failed #f)
(define (set-test-failed!)
  (set! test-failed #t))

(define *test-result-same?* (make-parameter equal?))

(define-syntax test
  (syntax-parser
    ((~and test-case (_ tested-expression expected-result))
     #'(begin
         (printf "Testing ~a\n" 'tested-expression)
         (let* ((expected expected-result)
                (produced tested-expression))
           (unless ((*test-result-same?*) expected produced)
             (set-test-failed!)
             (raise-syntax-error
              'test
              (format "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                      'tested-expression expected produced)
              #'test-case)))))))


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
