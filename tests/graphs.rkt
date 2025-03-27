#lang racket

(require "./applications/proof.rkt"
         "./applications/grammars.rkt"
         "./interpreter/basics.rkt"
         plot)

(define paper-dir "../staged-mk-paper/")

(define (plot-appendo-sizes [sizes (in-range 100 500 10)] [out-file #f])
  (define-values (handwritten staged unstaged)
    (for/lists (handwritten staged unstaged)
               ([size sizes])
      (printf "Plotting appendo sizes ~a~%" size)
      (define-values (handwritten staged unstaged) (appendo-size-timed size))
      (values (list size handwritten)
              (list size staged)
              (list size unstaged))))
  (plot (list (lines handwritten #:color 'green #:label "Handwritten")
              (lines staged #:color 'blue #:label "Staged")
              (lines unstaged #:color 'red #:label "Unstaged"))
        #:x-label "List Size"
        #:y-label "Time (ms)"
        #:title "Handwritten/Staged/Unstaged Runtime vs List Size"
        #:out-file out-file))

(define (plot-timing-test [timing-range (in-range 0 1000 20)]
                          [out-file #f])
  (define dataset
    (for/list ([size timing-range])
      (printf "Testing size ~a~%" size)
      (cons size (get-timing-data size))))
  (plot (list (lines (map (lambda (ks) (list (first ks) (second ks))) dataset)
                     #:color 'red #:label "Unstaged")
              (lines (map (lambda (ks) (list (first ks) (third ks))) dataset)
                     #:color 'blue #:label "Staged"))
        #:x-label "Output Size"
        #:y-label "Time (ms)"
        #:title "Unstaged/Staged Runtime vs Output Size"
        #:out-file out-file))

(define (proof-chart [sizes (in-range 1 5)] [filename #f])
  (define-values (unstaged staged)
    (for/lists (unstaged staged)
               ([size sizes])
      (printf "Timing proof of size ~a~%" size)
      (define-values (unstaged staged) (time-proof-of-size size))
      (values (list size unstaged) (list size staged))))
  (parameterize ([plot-x-ticks (fraction-ticks #:divisors '(1))])
    (plot (list (lines unstaged #:color 'red #:label "Unstaged")
                (lines staged #:color 'blue #:label "Staged"))
          #:x-label "Proof Size"
          #:y-label "Time (ms)"
          #:title "Unstaged/Staged Runtime vs Proof Size"
          #:out-file filename)))

(module+ main
  (define plot-dir
    (if (directory-exists? paper-dir)
        paper-dir
        "./bench-results/"))

  (unless (directory-exists? plot-dir)
    (make-directory plot-dir))

  (printf "PLOT DIR: ~a~%" plot-dir)

  (plot-timing-test (in-range 0 1000 20) (string-append plot-dir "grammars.svg"))
  (proof-chart (in-range 1 10) (string-append plot-dir "proofs.svg"))
  (plot-appendo-sizes (in-range 100 500 10) (string-append plot-dir "appendo.svg")))
