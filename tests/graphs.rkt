#lang racket

(require "./applications/proof.rkt"
         "./applications/grammars.rkt"
         "./interpreter/basics.rkt"
         plot)

(define paper-dir "../../../staged-mk-paper/")

(define (main)
  (define plot-dir
    (string-append (if (file-exists? paper-dir)
                       paper-dir
                       "../")
                   "bench-results/"))

  (unless (directory-exists? plot-dir)
    (make-directory plot-dir))

  (plot-timing-test (in-range 0 1000 20) (string-append plot-dir "grammars.svg"))
  (proof-chart (in-range 1 10) (string-append plot-dir "proofs.svg"))
  (plot-appendo-sizes (in-range 100 500 10) (string-append plot-dir "appendo.svg")))

(main)
