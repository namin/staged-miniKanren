#lang racket/base

(require "../../all.rkt" racket/list plot)

(provide plot-appendo-sizes)

(test
    (length
     (run 1 (q)
       (staged
        (evalo-staged `(lambda (x) x) q))))
  1)

(test
    (run 1 (q)
      (staged
       (evalo-staged `((lambda (x) x) 1) q)))
  '(1))

(test
    (length
     (run 1 (q)
       (staged
        (evalo-staged `(cons 1 2) q))))
  1)

(test
    (length
     (run 1 (q)
       (staged
        (evalo-staged `(lambda (f) (f 1)) q))))
  1)

(test
    (run 1 (q)
      (staged
       (evalo-staged
        `((lambda (f) (f 1))
          (lambda (x) x))
        q)))
  '(1))

(test
    (run 1 (q)
      (staged
       (evalo-staged
        `(letrec ((f (lambda (x) x)))
           (f 1))
        q)))
  '(1))

#;
(test
    (length
     (run 1 (q)
       (fresh (v)
         (evalo-staged
          q
          v))))
  1)

(test
    (run 1 (q)
      (staged
       (evalo-staged
        `(and #f #t)
        q)))
  '(#f))

(test
    (run 1 (q)
      (staged
       (evalo-staged
        `(and (and #t #t) (and #t #t))
        q)))
  '(#t))

(test
    (run 1 (q)
      (staged
       (fresh (x)
         (evalo-staged
          `(and ,x #t)
          #f))))
  '(_.0))

(test
    (length
     (run 2 (q)
       (staged
        (fresh (x y)
          (== (list x y) q)
          (evalo-staged
           `(and ,x #t)
           y)))))
  2)

#;(test
    (run 2 (q)
      (staged
       (evalo-staged
        `(and ,q #t)
        q)))
   '(#t #f))

(test
    ;; run 4 should generate a quine
    (run 3 (q)
      (staged
       (evalo-staged
        `(and #t ,q)
        q)))
  '((_.0 $$ (num _.0)) #t #f))

(test
    (run 3 (q)
      (staged
       (evalo-staged
        `(or #t . ,q)
        #t)))
  '(() (_.0 . _.1)))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (evalo-staged
          `(or #f . ,q)
          1)
         (later (== '(1) q)))))
  '((1)))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (evalo-staged
          `(or #f . ,q)
          1)
         (later (== '(#f 1) q)))))
  '((#f 1)))

(defrel (eval-on-append-synth-base q)
  (staged
   (evalo-staged
	`(letrec ((append
			   (lambda (xs ys)
				 (if (null? xs) ,q
					 (cons (car xs) (append (cdr xs) ys))))))
	   (append '(1 2) '(3 4)))
	'(1 2 3 4))))

(test
  (run 1 (q)
	(eval-on-append-synth-base q))
  '(ys))

(test
  (run 1 (q)
	(evalo-unstaged
	 `(letrec ((append
				(lambda (xs ys)
				  (if (null? xs) ,q
					  (cons (car xs) (append (cdr xs) ys))))))
		(append '(1 2) '(3 4)))
	 '(1 2 3 4)))
  '(ys))



(test
    (run 1 (q)
      (staged
       (fresh ()
         (evalo-staged `(,q (list 1 2)) 1)
         (later (==  q 'car)))))
  '(car))

(test
    (run* (q)
      (staged
       (fresh ()
         (later (== q 'car))
         (evalo-staged `(,q (list 1 2)) 1))))
  '(car))

(test
    (length
     (run 1 (q)
       (staged
        (fresh (q1 q2)
          (later (== q `(,q1 ,q2)))
          (evalo-staged `(,q1 (list ,q2 2)) q2)))))
  1)

(test
    (length
     (run 1 (q1 q2)
       (staged
        (evalo-staged `(,q1 (list ,q2 2)) q2))))
  1)

(defrel (appendo xs ys zs)
  (conde
    ((== xs '()) (== ys zs))
    ((fresh (xa xd zd)
       (== (cons xa xd) xs)
       (== (cons xa zd) zs)
       (appendo xd ys zd)))))

(test
    (run* (xs ys)
      (appendo xs ys '(a b c)))
  '((() (a b c)) ((a) (b c)) ((a b) (c)) ((a b c) ())))

(defrel (ex e)
  (staged
   (evalo-staged `(cons ,e '()) '(5))))

(defrel (appendo-unstaged xs ys zs)
  (evalo-unstaged
   `(letrec ((append
              (lambda (xs ys)
                (if (null? xs)
                    ys
                    (cons (car xs)
                          (append (cdr xs) ys))))))
      (append ',xs ',ys))
   zs))


(defrel (appendo-staged xs ys zs)
  (staged
   (evalo-staged
    `(letrec ((append
               (lambda (xs ys)
                 (if (null? xs)
                     ys
                     (cons (car xs)
                           (append (cdr xs) ys))))))
       (append ',xs ',ys))
    zs)))

(define-syntax-rule (time-form body ...)
  (let-values ([(_ __ wall-clock-time ___)
                (time-apply
                 (lambda ()
                   body ...)
                 '())])
    wall-clock-time))

(define (appendo-size-timed size)
  (define lst (range size))
  (values
   (time-form (run* (p q) (appendo p q lst)))
   (time-form (run* (p q) (appendo-staged p q lst)))
   (time-form (run* (p q) (appendo-unstaged p q lst)))))

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


(test
  (last-pair (run* (xs ys)
	(appendo-staged ys xs '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
							  AA AB AC AD AE AF AG AH AI AJ AK AL AM AN AO AP AQ AR AS AT AU AV AW AX AY AZ
							  BA BB BC BD BE BF BG BH BI BJ BK BL BM BN BO BP BQ BR BS BT BU BV BW BX BY BZ
							  CA CB CC CD CE CF CG CH CI CJ CK CL CM CN CO CP CQ CR CS CT CU CV CW CX CY CZ
							  DA DB DC DD DE DF DG DH DI DJ DK DL DM DN DO DP DQ DR DS DT DU DV DW DX DY DZ
							  EA EB EC ED EE EF EG EH EI EJ EK EL EM EN EO EP EQ ER ES ET EU EV EW EX EY EZ
							  FA FB FC FD FE FF FG FH FI FJ FK FL FM FN FO FP FQ FR FS FT FU FV FW FX FY FZ
							  GA GB GC GD GE GF GG GH GI GJ GK GL GM GN GO GP GQ GR GS GT GU GV GW GX GY GZ
							  HA HB HC HD HE HF HG HH HI HJ HK HL HM HN HO HP HQ HR HS HT HU HV HW HX HY HZ
							  IA IB IC ID IE IF IG IH II IJ IK IL IM IN IO IP IQ IR IS IT IU IV IW IX IY IZ
							  JA JB JC JD JE JF JG JH JI JJ JK JL JM))))
'((()
   (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
	AA AB AC AD AE AF AG AH AI AJ AK AL AM AN AO AP AQ AR AS AT AU AV AW AX AY AZ
	BA BB BC BD BE BF BG BH BI BJ BK BL BM BN BO BP BQ BR BS BT BU BV BW BX BY BZ
	CA CB CC CD CE CF CG CH CI CJ CK CL CM CN CO CP CQ CR CS CT CU CV CW CX CY CZ
	DA DB DC DD DE DF DG DH DI DJ DK DL DM DN DO DP DQ DR DS DT DU DV DW DX DY DZ
	EA EB EC ED EE EF EG EH EI EJ EK EL EM EN EO EP EQ ER ES ET EU EV EW EX EY EZ
	FA FB FC FD FE FF FG FH FI FJ FK FL FM FN FO FP FQ FR FS FT FU FV FW FX FY FZ
	GA GB GC GD GE GF GG GH GI GJ GK GL GM GN GO GP GQ GR GS GT GU GV GW GX GY GZ
	HA HB HC HD HE HF HG HH HI HJ HK HL HM HN HO HP HQ HR HS HT HU HV HW HX HY HZ
	IA IB IC ID IE IF IG IH II IJ IK IL IM IN IO IP IQ IR IS IT IU IV IW IX IY IZ
	JA JB JC JD JE JF JG JH JI JJ JK JL JM))))


(test
  (last-pair (run* (xs ys)
	(appendo-unstaged ys xs '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
							  AA AB AC AD AE AF AG AH AI AJ AK AL AM AN AO AP AQ AR AS AT AU AV AW AX AY AZ
							  BA BB BC BD BE BF BG BH BI BJ BK BL BM BN BO BP BQ BR BS BT BU BV BW BX BY BZ
							  CA CB CC CD CE CF CG CH CI CJ CK CL CM CN CO CP CQ CR CS CT CU CV CW CX CY CZ
							  DA DB DC DD DE DF DG DH DI DJ DK DL DM DN DO DP DQ DR DS DT DU DV DW DX DY DZ
							  EA EB EC ED EE EF EG EH EI EJ EK EL EM EN EO EP EQ ER ES ET EU EV EW EX EY EZ
							  FA FB FC FD FE FF FG FH FI FJ FK FL FM FN FO FP FQ FR FS FT FU FV FW FX FY FZ
							  GA GB GC GD GE GF GG GH GI GJ GK GL GM GN GO GP GQ GR GS GT GU GV GW GX GY GZ
							  HA HB HC HD HE HF HG HH HI HJ HK HL HM HN HO HP HQ HR HS HT HU HV HW HX HY HZ
							  IA IB IC ID IE IF IG IH II IJ IK IL IM IN IO IP IQ IR IS IT IU IV IW IX IY IZ
							  JA JB JC JD JE JF JG JH JI JJ JK JL JM))))
'((()
   (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
	AA AB AC AD AE AF AG AH AI AJ AK AL AM AN AO AP AQ AR AS AT AU AV AW AX AY AZ
	BA BB BC BD BE BF BG BH BI BJ BK BL BM BN BO BP BQ BR BS BT BU BV BW BX BY BZ
	CA CB CC CD CE CF CG CH CI CJ CK CL CM CN CO CP CQ CR CS CT CU CV CW CX CY CZ
	DA DB DC DD DE DF DG DH DI DJ DK DL DM DN DO DP DQ DR DS DT DU DV DW DX DY DZ
	EA EB EC ED EE EF EG EH EI EJ EK EL EM EN EO EP EQ ER ES ET EU EV EW EX EY EZ
	FA FB FC FD FE FF FG FH FI FJ FK FL FM FN FO FP FQ FR FS FT FU FV FW FX FY FZ
	GA GB GC GD GE GF GG GH GI GJ GK GL GM GN GO GP GQ GR GS GT GU GV GW GX GY GZ
	HA HB HC HD HE HF HG HH HI HJ HK HL HM HN HO HP HQ HR HS HT HU HV HW HX HY HZ
	IA IB IC ID IE IF IG IH II IJ IK IL IM IN IO IP IQ IR IS IT IU IV IW IX IY IZ
	JA JB JC JD JE JF JG JH JI JJ JK JL JM))))


(test
    (run 1 (q)
      (staged
       (fresh ()
         (later (== q '(lambda () (lambda (x) x))))
         (evalo-staged `((,q) 1) 1))))
      
  '((lambda () (lambda (x) x))))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (evalo-staged `((,q) 1) 1)
         (later (== q '(lambda () (lambda (x) x)))))))
      
    '((lambda () (lambda (x) x))))

(test
    (run 1 (q)
      (evalo-unstaged `(((lambda () car)) (cons 1 2)) 1))
      
  '(_.0))

(test
    (run 1 (q)
      (staged
       (evalo-staged `((lambda () car)) q)))
      
  '((struct prim . car)))

(test
    (run 1 (q)
      (staged
       (evalo-staged `(((lambda () car)) (cons 1 2)) 1)))
      
  '(_.0))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (== q '(lambda () car))
         (evalo-staged `((,q) (cons 1 2)) 1))))
      
  '((lambda () car)))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (later (== q '(lambda () car)))
         (evalo-staged `((,q) (cons 1 2)) 1))))
      
    '((lambda () car)))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (evalo-staged `((,q) (cons 1 2)) 1)
         (later (== q '(lambda () car))))))
      
  '((lambda () car)))

(test
    (run 1 (q)
      (staged
       (fresh ()
         (symbolo q)
         (later (numbero q)))))
  '())

(defrel (bogus-appendo xs ys zs)
  (staged
   (evalo-staged
    `(letrec ((append
               (lambda (xs ys)
                 (if (null? xs)
                     ys
                     (cons (car xs)
                           (append (cdr xs) ys))))))
       append)
    zs)))

;; TODO: is this weird?
;;       it's not understood by evalo-unstaged
(test
    (length (run 1 (q)
              (bogus-appendo '(1 2) '(3 4) q)))
  1)

(test
    (run 1 (q)
      (staged
       (evalo-staged '(list 1 2 3) q)))
  '((1 2 3)))

;; Because this is a cross-stage test, and the staged and unstaged
;; are now separate, use the runtime interp generated from staged instead
;; of evalo-unstaged.
(test
    (length
     (run 1 (q p e)
       (staged
        (fresh ()
          (eval-expo `(x 1) `((x . (val . ,p))) q)
          (later (evalo-staged
                  `(letrec ((f ,e)) f) p))))))
  1)

;; With new fallback search this now specializes and our bug with losing substitution
;; extensions from capture-later means we lose the fact that the var x must be the
;; symbol x
(todo "remember lambda argument name"
    (run 1 (q x)
      (staged
       (fresh ()
         (symbolo x)
         (evalo-staged `((lambda (,x) x) 1) q))))
  '((1 x)))

(test
    (length
     (run 3 (q x y)
       (staged
        (fresh ()
          (symbolo x)
          (symbolo y)
          (=/= x 'lambda)
          (=/= y 'lambda)
          (evalo-staged `(((lambda (,x) (lambda (,y) z)) 1) 2) q)))))
  2)
;; NOTE: res has a u-lookupo call!

(test
    (run 1 (q)
      (staged
       (evalo-staged '(match '(hello) [`(hello ,x) 1]) q)))
  '())
  

(test
    (run 1 (q)
      (staged
       (evalo-staged '(match '(hello) [`(hello ,x) 1] [`(,x) 2]) q)))
  '(2))
  

;; regression test---this raised a problem when reflecting datums to quasiquotes
;; because fix-scope special cased `quote` and didn't understand quasiquote.
(test
 (run 1 (q) (staged (later (evalo-unstaged `(quote ,q) 5))))
 '(5))
