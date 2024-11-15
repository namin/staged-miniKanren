#lang racket

(require "../../main.rkt"
         "../../test-check.rkt")

(provide plot-timing-test)

#|
Grammar syntax:
- (or A B C) is disjunction
- (seq A B C) is sequencing
- (quote A) (where A is a symbol) refers to nonterminal named A
- A (where A is a symbol/number) refers to the terminal written 'A'
|#

(define grammar
  '((E . (or (seq 'S * 'S)
             'S))
    (S . (or (seq 'T + 'T)
             'T))
    (T . (or 0
             (seq < 'E >)))))

(defrel/staged (lookupo map key res)
  (fresh (first rest)
    (== map (cons first rest))
    (conde
     [(== first `(,key . ,res))]
     [(lookupo rest key res)])))

(defrel/staged (appendo xs ys res)
  (conde
   [(== xs '()) (== ys res)]
   [(fresh (a d resp)
      (== xs (cons a d))
      (appendo d ys resp)
      (== res (cons a resp)))]))

#|
This is the version I'd like to write - I have two staging-time parameters,
the grammar and the expression I'm interpreting. The one run-time parameter,
then, is true only of strings which fit the given expression within the given grammar.

It works fine normally, but staging it doesn't terminate. We'd like to use the
recursive parameter, but we can't, as the relation is already specialized on the
expression to be evalued.

(defrel-partial/staged (interp-grammar rel [grammar expr] [res])
  (conde
   [(conde [(symbolo expr)] [(numbero expr)])
    (later (== res (list expr)))]
   [(fresh (first rest c1 c2)
      (== expr `(or ,first . ,rest))
      (specialize-partial-apply c1 interp-grammar grammar first)
      (specialize-partial-apply c2 interp-grammar grammar `(or . ,rest))
      (later
       (conde
        [(finish-apply c1 interp-grammar res)]
        [(finish-apply c2 interp-grammar res)])))]
   [(== expr '(seq))
    (later (== res '()))]
   [(fresh (first rest c1 c2)
      (== expr `(seq ,first . ,rest))
      (specialize-partial-apply c1 interp-grammar grammar first)
      (specialize-partial-apply c2 interp-grammar grammar `(seq . ,rest))
      (later
       (fresh (a d)
         (finish-apply c1 interp-grammar a)
         (finish-apply c2 interp-grammar d)
         (appendo a d res))))]
   [(fresh (nt ref c)
      (== expr `(quote ,nt))
      (lookupo grammar nt ref)
      (specialize-partial-apply c interp-grammar grammar ref)
      (later
       (finish-apply c interp-grammar res)))]))

(defrel/staged (interp-E res)
  (fresh (c)
    (specialize-partial-apply c interp-grammar grammar ''E)
    (later (finish-apply c interp-grammar res))))
|#

(defrel-partial/staged (interp-nt rel [grammar] [nt res])
  (gather
   (fresh (expr)
     (lookupo grammar nt expr)
     (interp-grammar rel expr res))))

(defrel/staged (interp-grammar nts expr res)
  (conde
   [(conde [(symbolo expr)] [(numbero expr)])
    (== res (list expr))]
   [(fresh (first rest)
      (== expr `(or ,first . ,rest))
      (conde
       [(interp-grammar nts first res)]
       [(interp-grammar nts `(or . ,rest) res)]))]
   [(== expr '(seq))
    (== res '())]
   [(fresh (first rest c1 c2 c3)
      (== expr `(seq ,first . ,rest))
      (interp-grammar nts first c1)
      (interp-grammar nts `(seq . ,rest) c2)
      (later (appendo c1 c2 res)))]
   [(fresh (nt)
      (== expr `(quote ,nt))
      (later (finish-apply nts interp-nt nt res)))]))

(defrel/staged (interp-E res)
  (fresh (rel)
    (specialize-partial-apply rel interp-nt grammar)
    (later (finish-apply rel interp-nt 'E res))))

(parameterize ([*test-result-same?* set=?])
  (test (run 4 (q r s)
          (interp-E `(< 0 ,q ,r ,s)))
        '((+ 0 >) (> + 0) (* 0 >) (> * 0)))
  (test (run 4 (q r s)
          (staged (interp-E `(< 0 ,q ,r ,s))))
        '((+ 0 >) (> + 0) (* 0 >) (> * 0)))
  (pretty-print (generated-code)))

(let ((size 200))
  (record-bench 'unstaged 'grammar-synthesis)
  (time
   (run size (r)
     (interp-E r)))
  (record-bench 'run-staged 'grammar-synthesis)
  (time
   (run size (r)
     (staged (interp-E r)))))

(define (get-timing-data size)
  (define-values (ures ucpu ureal ugc)
    (time-apply
     (lambda ()
       (run size (r)
         (interp-E r)))
     '()))
  (define-values (sres scpu sreal sgc)
    (time-apply
     (lambda ()
       (run size (r)
         (staged (interp-E r))))
     '()))
  (list ureal sreal))

(require plot)
(plot-new-window? #t)

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
