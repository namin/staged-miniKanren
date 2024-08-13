#lang racket/base

(require "all.rkt"
         racket/list
         (only-in "staged-load.rkt"
                  convert-constraints
                  maybe-remove-constraints
                  unique-result
                  reified-expand
                  fix-scope-syntax
                  [fresh internal-fresh]))


;; Test the correspondence of the staged and unstaged interpreters by generating many
;; expr, val pairs using the unstaged interpreter and verifying that the staged interpreter
;; checks the same pairs successfully.


;; (-> RunResult RunResult)
;;
;; Given a single result from an execution of the unstaged interpreter, including
;; the pair (expr val) as the term and possibly including constraints, execute an
;; equivalent query in the unstaged interpreter and return the result.
(define (shake1 run-result)
  (define expr+val (maybe-remove-constraints run-result))
  (define constraint-goals (convert-constraints run-result))

  (define staged-query-stx
    #`(run 2 (expr val)
           (staged
            ;; hack: we are generating a query to evaluate in the surface langauge
            ;; so we want references to surface language constraints, not the references
            ;; to internal language constraints we get from convert-constraints.
            ;; A similar issue with the internal-fresh.
            #,(datum->syntax #'here
                             (syntax->datum
                              (fix-scope-syntax
                               #`(internal-fresh ()
                                                 #,@constraint-goals
                                                 (== #,(reified-expand (car expr+val)) expr)
                                                 (== #,(reified-expand (cadr expr+val)) val)
                                                 (evalo-staged expr val))))))))
  
  ;; The query is a run 2, but we expect a single result. A given unstaged interpreter may
  ;; be somewhat general; e.g. containing logic variables; the staged interpreter should
  ;; generalize in the same way.
  (unique-result
   (eval-syntax
    staged-query-stx)))

(module+ main
  ;; Generate many (expr val) results from the unstaged interpreter
  (define ts
    (run 300 (expr val)
         ;; We currently can't reflect apply-rep values to syntax, so don't
         ;; use values with closures as tests.
         (absento 'closure val)
         (evalo-unstaged expr val)))

  ;; For each unstaged interpreter result, see if the staged interpreter produces the same
  ;; result.
  (define rs
    (for/list ([t ts] [i (range (length ts))])
      (printf "Shake ~a\n" i)
      (shake1 t)))

  ;; The expr, val mapping from the staged interpreter should be the same as the unstaged interpreter
  ;; in all cases.
  (test
   rs
   ts))
