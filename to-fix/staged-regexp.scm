;; inspired by https://github.com/webyrd/relational-parsing-with-derivatives

(define regexp-NULL #f)    ; -- the empty set
(define regexp-BLANK #t)   ; -- the empty string

(define (valid-seqo exp)
  (fresh (pat1 pat2)
    (== `(seq ,pat1 ,pat2) exp)
    (=/= regexp-NULL pat1)
    (=/= regexp-BLANK pat1)
    (=/= regexp-NULL pat2)
    (=/= regexp-BLANK pat2)))

(define (seqo pat1 pat2 out)
  (conde
    [(== regexp-NULL pat1) (== regexp-NULL out)]
    [(== regexp-NULL pat2) (== regexp-NULL out) (=/= regexp-NULL pat1)]
    [(== regexp-BLANK pat1) (== pat2 out) (=/= regexp-NULL pat2)]
    [(== regexp-BLANK pat2) (== pat1 out) (=/= regexp-NULL pat1) (=/= regexp-BLANK pat1)]
    [(=/= regexp-NULL pat1) (=/= regexp-BLANK pat1) (=/= regexp-NULL pat2) (=/= regexp-BLANK pat2) (== `(seq ,pat1 ,pat2) out)]))

(define (valid-alto exp)
  (fresh (pat1 pat2)
    (== `(alt ,pat1 ,pat2) exp)
    (=/= regexp-NULL pat1)
    (=/= regexp-NULL pat2)
    (=/= pat1 pat2)))

(define (alto pat1 pat2 out)
  (conde
    [(== pat1 pat2) (== pat1 out)]
    [(=/= pat1 pat2)
     (conde
       [(== regexp-NULL pat1) (== pat2 out)]
       [(== regexp-NULL pat2) (== pat1 out) (=/= regexp-NULL pat1)]
       [(=/= regexp-NULL pat1) (=/= regexp-NULL pat2) (== `(alt ,pat1 ,pat2) out)])]))

(define (valid-repo exp)
  (fresh (pat)
    (== `(rep ,pat) exp)
    (=/= regexp-BLANK pat)
    (=/= regexp-NULL pat)
    (fresh (re1 re2)
      (conde
        ((symbolo pat))
        ((== `(seq ,re1 ,re2) pat))
        ((== `(alt ,re1 ,re2) pat))))))

(define (repo pat out)
  (conde
    [(== regexp-BLANK out)
     (conde
       [(== regexp-NULL pat)]
       [(== regexp-BLANK pat)])]
    [(conde
       ((symbolo pat) (== `(rep ,pat) out))
       ((fresh (re1 re2)
          (conde
            ((== `(rep ,re1) pat)
             ; remove nested reps
             (== pat out))
            ((== `(seq ,re1 ,re2) pat)
             (== `(rep ,pat) out))
            ((== `(alt ,re1 ,re2) pat)
             (== `(rep ,pat) out))))))]))

(define (regexp-matcho pattern data out)
  (conde
    ((== '() data) (deltao pattern out))
    ((fresh (a d res)
       (== (cons a d) data)
       (derivo pattern a res)
       (regexp-matcho res d out)))))

(define (deltao re out)
  (conde
    [(== regexp-BLANK re) (== #t out)]
    [(== regexp-NULL re) (== #f out)]
    [(symbolo re) (== #f out)]
    [(fresh (re1)
       (== `(rep ,re1) re)
       (== #t out)
       (valid-repo re))]
    [(fresh (re1 re2 res1 res2)
       (== `(seq ,re1 ,re2) re)
       (valid-seqo re)
       (conde
         ((== #f res1) (== #f out))
         ((== #t res1) (== #f res2) (== #f out))
         ((== #t res1) (== #t res2) (== #t out)))
       (deltao re1 res1)
       (deltao re2 res2))]
    [(fresh (re1 re2 res1 res2)
       (== `(alt ,re1 ,re2) re)
       (valid-alto re)
       (conde
         ((== #t res1) (== #t out))
         ((== #f res1) (== #t res2) (== #t out))
         ((== #f res1) (== #f res2) (== #f out)))
       (deltao re1 res1)
       (deltao re2 res2))]))

(define (derivo re c out)
  (fresh ()
    (symbolo c)
    (conde
      [(== regexp-BLANK re) (== regexp-NULL out)]
      [(== regexp-NULL re) (== regexp-NULL out)]
      [(symbolo re)
       (conde
         [(l== c re) (== regexp-BLANK out)]
         [(=/= c re) (== regexp-NULL out)])]
      [(fresh (re1 res1)
         (== `(rep ,re1) re)
         (valid-repo re)
         (seqo res1 re out)
         (derivo re1 c res1))]
      [(fresh (re1 re2 res1 res2)
         (== `(alt ,re1 ,re2) re)
         (valid-alto re)
         (derivo re1 c res1)
         (derivo re2 c res2)
         (alto res1 res2 out))]
      [(fresh (re1 re2 res1 res2 res3 res4 res5)
         (== `(seq ,re1 ,re2) re)
         (valid-seqo re)
         (derivo re1 c res1)
         (deltao re1 res3)
         (derivo re2 c res4)
         (seqo res1 re2 res2)
         (seqo res3 res4 res5)
         (alto res2 res5 out))])))
