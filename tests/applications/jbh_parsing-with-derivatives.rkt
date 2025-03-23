#lang racket/base

(require "../../all.rkt")

;; Adapted from Matt Might's code for parsing with derivatives.
;; Cf https://dl.acm.org/doi/10.1145/2034773.2034801

;; Template for derivatives of regex wrt characters, and doing regex matching based off that
(define-term-syntax-rule
  (parse body-expr)
  `(letrec ((regex-NULL (lambda () #f))) ;; constructor for always-fail regex
     (letrec ((regex-BLANK (lambda () #t))) ;; constructor for never-fail regex
       (letrec ((regex-alt?
                 (lambda (re) (and (pair? re) (equal? (car re) 'alt)))))
         (letrec ((regex-seq?
                   (lambda (re) (and (pair? re) (equal? (car re) 'seq)))))
           (letrec ((regex-rep?
                     (lambda (re) (and (pair? re) (equal? (car re) 'rep)))))
             (letrec ((regex-null? (lambda (re) (equal? re #f))))
               (letrec ((regex-empty? (lambda (re) (equal? re #t))))
                 (letrec ((regex-atom? (lambda (re) (symbol? re))))
                   (letrec ((match-seq
                             (lambda (re f)
                               (and (regex-seq? re)
                                    (f (car (cdr re)) (car (cdr (cdr re))))))))
                     (letrec ((match-alt
                               (lambda (re f)
                                 (and (regex-alt? re)
                                      (f (car (cdr re)) (car (cdr (cdr re))))))))
                       (letrec ((match-rep
                                 (lambda (re f)
                                   (and (regex-rep? re) (f (car (cdr re)))))))
						 ;; Smart constructors that simplify as they go
                         (letrec ((seq
                                   (lambda (pat1 pat2)
                                     (if (regex-null? pat1)
                                         (regex-NULL)
                                         (if (regex-null? pat2)
                                             (regex-NULL)
                                             (if (regex-empty? pat1)
                                                 pat2
                                                 (if (regex-empty? pat2)
                                                     pat1
                                                     (list 'seq pat1 pat2))))))))
                           (letrec ((alt
                                     (lambda (pat1 pat2)
                                       (if (regex-null? pat1)
                                           pat2
                                           (if (regex-null? pat2)
                                               pat1
                                               (list 'alt pat1 pat2))))))
                             (letrec ((rep
                                       (lambda (pat)
                                         (if (regex-null? pat)
                                             (regex-BLANK)
                                             (if (regex-empty? pat)
                                                 (regex-BLANK)
                                                 (list 'rep pat))))))
                               (letrec ((regex-empty
                                         (lambda (re)
                                           (if (regex-empty? re)
                                               #t
                                               (if (regex-null? re)
                                                   #f
                                                   (if (regex-atom? re)
                                                       #f
                                                       (or (match-seq
                                                            re
                                                            (lambda (pat1 pat2)
                                                              (seq
                                                               (regex-empty pat1)
                                                               (regex-empty pat2))))
                                                           (match-alt
                                                            re
                                                            (lambda (pat1 pat2)
                                                              (alt
                                                               (regex-empty pat1)
                                                               (regex-empty pat2))))
                                                           (if (regex-rep? re)
                                                               #t
                                                               #f))))))))
                                 (letrec ((d/dc
                                           (lambda (re c)
                                             (if (regex-empty? re)
                                                 (regex-NULL)
                                                 (if (regex-null? re)
                                                     (regex-NULL)
                                                     (if (equal? c re)
                                                         (regex-BLANK)
                                                         (if (regex-atom? re)
                                                             (regex-NULL)
                                                             (or (match-seq
                                                                  re
                                                                  (lambda (pat1 pat2)
                                                                    (alt
                                                                     (seq
                                                                      (d/dc pat1 c)
                                                                      pat2)
                                                                     (seq
                                                                      (regex-empty pat1)
                                                                      (d/dc pat2 c)))))
                                                                 (match-alt
                                                                  re
                                                                  (lambda (pat1 pat2)
                                                                    (alt
                                                                     (d/dc pat1 c)
                                                                     (d/dc pat2 c))))
                                                                 (match-rep
                                                                  re
                                                                  (lambda (pat)
                                                                    (seq
                                                                     (d/dc pat c)
                                                                     (rep pat))))
                                                                 (regex-NULL)))))))))
                                   (letrec ((regex-match
                                             (lambda (pattern data)
                                               (if (null? data)
                                                   (regex-empty?
                                                    (regex-empty pattern))
                                                   (regex-match
                                                    (d/dc pattern (car data))
                                                    (cdr data))))))

                                     ,body-expr

                                     ))))))))))))))))))

(defrel (d/dc-o re c parse-result)
  (time-staged
   (evalo-staged
    (parse `(d/dc ',re ',c))
    parse-result)))

(record-bench 'eval-eval 'staging 'regex-match #:description "Check that a string matches a regex")
(defrel (regex-matcho pattern data parse-result)
  (time-staged
   (evalo-staged
    (parse `(regex-match ',pattern ',data))
    parse-result)))


(test

  (run #f (parse-result)
    (d/dc-o 'baz 'f parse-result))
  '(#f))


(test

  (run #f (parse-result)
    (evalo-unstaged
      (parse '(d/dc 'baz 'f))
      parse-result))
  '(#f))


(test
  (run #f (parse-result)
    (d/dc-o '(seq foo barn) 'foo parse-result))
  '(barn))

(test
  (run #f (parse-result)
    (evalo-unstaged
      (parse '(d/dc '(seq foo barn) 'foo))
      parse-result))
  '(barn))

(test
  (run #f (parse-result)
    (d/dc-o '(alt (seq foo bar) (seq foo (rep baz))) 'foo parse-result))
  '((alt bar (rep baz))))

(test
  (run #f (parse-result)
    (evalo-unstaged
      (parse '(d/dc '(alt (seq foo bar) (seq foo (rep baz))) 'foo))
      parse-result))
  '((alt bar (rep baz))))

;; runs match sequence forward against ground input
(test
;;  #:times 100
  (run 1 (parse-result)
    (regex-matcho '(seq foo (rep bar)) 
                  '(foo bar bar bar)
                  parse-result))
  '(#t))

(test
;;  #:times 100
  (run #f (parse-result)
    (evalo-unstaged
      (parse '(regex-match '(seq foo (rep bar)) 
                           '(foo bar bar bar)))
      parse-result))
  '(#t))

;; runs match sequence forward against ground input that fails
(test
;;  #:times 100
  (run 1 (parse-result)
    (regex-matcho '(seq foo (rep bar)) 
                  '(foo bar baz bar bar)
                  parse-result))
  '(#f))

(test
;;  #:times 100
  (run #f (parse-result)
    (evalo-unstaged
      (parse '(regex-match '(seq foo (rep bar)) 
                          '(foo bar baz bar bar)))
      parse-result))
  '(#f))

(record-bench 'eval-eval 'staged 'regex-match 1)
(time-test
  #:times 100
  (run 1 (parse-result)
    (regex-matcho '(seq foo (rep (alt bar baz))) 
                  '(foo bar baz bar bar)
                  parse-result))
  '(#t))

;; runs match sequence w/alt forward against ground input
(record-bench 'eval-eval 'unstaged 'regex-match 1 #:description "Check that a string matches a regex (x100)")
(time-test
  #:times 100
  (run 1 (parse-result)
    (evalo-unstaged
     (parse '(regex-match '(seq foo (rep (alt bar baz)))
                          '(foo bar baz bar bar)))
      parse-result))
  '(#t))


(record-bench 'eval-eval 'staged 'regex-match 2)
(time-test
  #:times 10
  (run 1 (regex)
    (regex-matcho regex
                  '(foo bar foo bar foo bar)
                  #t))
  '(((rep (seq foo bar . _.0) . _.1) $$ (absento (struct _.0) (struct _.1)))))


(record-bench 'eval-eval 'unstaged 'regex-match 2 #:description "Synth regex that matches a given string (x10)")
(time-test
  #:times 10
  (run 1 (regex)
    (evalo-unstaged
     (parse `(regex-match ',regex '(foo bar foo bar foo bar)))
      #t))
  '(((rep (seq foo bar . _.0) . _.1) $$ (absento (struct _.0) (struct _.1)))))




;; This is using the regex-derivative backwards
;;
;; the original regex running forward was the symbol 'baz'
(time-test
  (run 1 (regex)
	(absento #t regex)
    (absento #f regex)
    (d/dc-o regex 'a '(seq b c)))
  '(((seq a (seq b c) . _.0) $$ (absento (struct _.0) (#f _.0) (#t _.0)))))

(time-test
  (run 1 (regex)
	(absento #t regex)
    (absento #f regex)
    (evalo-unstaged
      (parse `(d/dc ',regex 'a))
      '(seq b c)))
  '(((seq a (seq b c) . _.0) $$ (absento (struct _.0) (#f _.0) (#t _.0)))))


;; the orginal regex running forward was '(seq foo barn)'
(test
  (run 1 (regex)
    (d/dc-o regex 'foo 'barn))
  `(((seq foo barn . _.0)
     $$
     ,absento-tags0)))

(test
  (run 1 (regex)
    (evalo-unstaged
      (parse `(d/dc ',regex 'foo))
      'barn))
  `(((seq foo barn . _.0)
     $$
     ,absento-tags0)))


(time-test
  (run 1 (regex)
    (d/dc-o regex 'foo '(alt bar (rep baz))))
  `(((seq foo (alt bar (rep baz)) . _.0)
     $$
     ,absento-tags0)))

(time-test
  (run 1 (regex)
    (evalo-unstaged
      (parse `(d/dc ',regex 'foo))
      '(alt bar (rep baz))))
  'timeout)
