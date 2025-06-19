#lang racket

(require "../../all.rkt")

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

(defrel (regex-matcho-unstaged pattern data parse-result)
  (evalo-unstaged
   (parse `(regex-match ',pattern ',data))
   parse-result))

(defrel (regex-matcho-staged pattern data parse-result)
  (time-staged
   (evalo-staged
    (parse `(regex-match ',pattern ',data))
    parse-result)))
#;(generated-code)

(time
 (run 1 (parse-result)
   (regex-matcho-unstaged '(seq foo (rep (alt bar baz))) 
                          '(foo bar baz bar bar)
                          parse-result)))

(time
 (run 1 (parse-result)
   (regex-matcho-staged '(seq foo (rep (alt bar baz))) 
                        '(foo bar baz bar bar)
                        parse-result)))


;; Takes ~9 seconds
(time
   (run 1 (regex)
     (regex-matcho-unstaged regex
                            '(foo bar foo bar foo bar)
                            #t)))

(time
 (run 1 (regex)
   (regex-matcho-staged regex
                        '(foo bar foo bar foo bar)
                        #t)))
