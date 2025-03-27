#lang racket

(require "../../main.rkt"
         "../../test-check.rkt")

(provide get-timing-data)

#|
Grammar syntax:
- (or A B C) is disjunction
- (seq A B C) is sequencing
- (quote A) (where A is a symbol) refers to nonterminal named A
- A (where A is a symbol/number) refers to the terminal written 'A'
|#

(define E-grammar
  '((E . (or 'S (seq 'S * 'S)))
    (S . (or 'T (seq 'T + 'T)))
    (T . (or 0 (seq < 'E >)))))

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

(defrel-partial/staged (interp-grammar rel [grammar] [nt res])
  (gather
   (fresh (expr)
     (lookupo grammar nt expr)
     (interp-rhs rel expr res))))

(defrel/staged (interp-rhs recur expr res)
  (conde
   [(conde [(symbolo expr)] [(numbero expr)])
    (== res (list expr))]
   [(fresh (first rest)
      (== expr `(or ,first . ,rest))
      (conde
       [(interp-rhs recur first res)]
       [(interp-rhs recur `(or . ,rest) res)]))]
   [(== expr '(seq))
    (== res '())]
   [(fresh (first rest c1 c2 c3)
      (== expr `(seq ,first . ,rest))
      (interp-rhs recur first c1)
      (interp-rhs recur `(seq . ,rest) c2)
      (later (appendo c1 c2 res)))]
   [(fresh (nt)
      (== expr `(quote ,nt))
      (later (finish-apply recur interp-grammar nt res)))]))

(defrel/staged (recognizeo grammar nt str)
  (fresh (rel)
    (specialize-partial-apply rel interp-grammar grammar)
    (later (finish-apply rel interp-grammar nt str))))

(defrel/staged (recognize-Eo str)
  (recognizeo E-grammar 'E str))

(parameterize ([*test-result-same?* set=?])
  (test (run 4 (q r s)
          (recognize-Eo `(< 0 ,q ,r ,s)))
        '((+ 0 >) (> + 0) (* 0 >) (> * 0)))
  (test (run 4 (q r s)
          (staged (recognize-Eo `(< 0 ,q ,r ,s))))
        '((+ 0 >) (> + 0) (* 0 >) (> * 0)))
  (pretty-print (generated-code)))

(record-bench 'simple 'staging 'grammar-synthesis)
(defrel (interp-E-staged r)
  (time-staged (recognize-Eo r)))

(let ((size 200))
  (record-bench 'simple 'unstaged 'grammar-synthesis #:description "Find 200 strings that match a given grammar as in \\cref{sec:parser}")
  (time
   (run size (str)
     (recognize-Eo str)))

  (record-bench 'simple 'staged 'grammar-synthesis)
  (time
   (run size (str)
     (interp-E-staged str))))

(define (get-timing-data size)
  (define-values (ures ucpu ureal ugc)
    (time-apply
     (lambda ()
       (run size (r)
         (recognize-Eo r)))
     '()))
  (define-values (sres scpu sreal sgc)
    (time-apply
     (lambda ()
       (run size (r)
         (staged (recognize-Eo r))))
     '()))
  (list ureal sreal))

(require plot)
(plot-new-window? #t)

