#lang racket/base

(require "../../all.rkt")



;; Or, square the x first and take to a half power.
;; Then you worry about duplicating the sqr expression, need
;; let insertion.

;; map (run <fn z => ~(power 2 <z>)>) [1,2,3,4,5]



(defrel/staged (appendo l s out)
  (conde
    [(== '() l) (== s out)]
    [(fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res))]))

(define build-num
  (lambda (n)
    (cond
      ((odd? n)
       (cons 1
         (build-num (quotient (- n 1) 2))))
      ((and (not (zero? n)) (even? n))
       (cons 0
         (build-num (quotient n 2))))
      ((zero? n) '()))))

(defrel/staged (zeroo n)
  (== '() n))

(defrel/staged (poso n)
  (fresh (a d)
    (== `(,a . ,d) n)))

(defrel/staged (>1o n)
  (fresh (a ad dd)
    (== `(,a ,ad . ,dd) n)))

(defrel/staged (full-addero b x y r c)
  (conde
    ((== 0 b) (== 0 x) (== 0 y) (== 0 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 0 b) (== 1 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 1 x) (== 0 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 0 x) (== 1 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 1 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 1 b) (== 1 x) (== 1 y) (== 1 r) (== 1 c))))

(defrel/staged (addero d n m r)
  (conde
    ((== 0 d) (== '() m) (== n r))
    ((== 0 d) (== '() n) (== m r)
     (poso m))
    ((== 1 d) (== '() m)
     (addero 0 n '(1) r))
    ((== 1 d) (== '() n) (poso m)
     (addero 0 '(1) m r))
    ((== '(1) n) (== '(1) m)
     (fresh (a c)
       (== `(,a ,c) r)
       (full-addero d 1 1 a c)))
    ((== '(1) n) (gen-addero d n m r))
    ((== '(1) m) (>1o n) (>1o r)
     (addero d '(1) n r))
    ((>1o n) (gen-addero d n m r))))

(defrel/staged (gen-addero d n m r)
  (fresh (a b c e x y z)
    (== `(,a . ,x) n)
    (== `(,b . ,y) m) (poso y)
    (== `(,c . ,z) r) (poso z)
    (full-addero d a b c e)
    (addero e x y z)))

(defrel/staged (pluso n m k)
  (addero 0 n m k))

(defrel/staged (minuso n m k)
  (pluso m k n))

(defrel/staged (*o n m p)
  (conde
    ((== '() n) (== '() p))
    ((poso n) (== '() m) (== '() p))
    ((== '(1) n) (poso m) (== m p))
    ((>1o n) (== '(1) m) (== n p))
    ((fresh (x z)
       (== `(0 . ,x) n) (poso x)
       (== `(0 . ,z) p) (poso z)
       (>1o m)
       (*o x m z)))
    ((fresh (x y)
       (== `(1 . ,x) n) (poso x)
       (== `(0 . ,y) m) (poso y)
       (*o m n p)))
    ((fresh (x y)
       (== `(1 . ,x) n) (poso x)
       (== `(1 . ,y) m) (poso y)
       (odd-*o x n m p)))))

(defrel/staged (odd-*o x n m p)
  (fresh (q)
    (bound-*o q p n m)
    (*o x m q)
    (pluso `(0 . ,q) m p)))

(defrel/staged (bound-*o q p n m)
  (conde
    ((== '() q) (poso p))
    ((fresh (a0 a1 a2 a3 x y z)
       (== `(,a0 . ,x) q)
       (== `(,a1 . ,y) p)
       (conde
         ((== '() n)
          (== `(,a2 . ,z) m)
          (bound-*o x y z '()))
         ((== `(,a3 . ,z) n)
          (bound-*o x y z m)))))))

(defrel/staged (=lo n m)
  (conde
    ((== '() n) (== '() m))
    ((== '(1) n) (== '(1) m))
    ((fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (=lo x y)))))

(defrel/staged (<lo n m)
  (conde
    ((== '() n) (poso m))
    ((== '(1) n) (>1o m))
    ((fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (<lo x y)))))

(defrel/staged (<=lo n m)
  (conde
    ((=lo n m))
    ((<lo n m))))

(defrel/staged (<o n m)
  (conde
    ((<lo n m))
    ((=lo n m)
     (fresh (x)
       (poso x)
       (pluso n x m)))))

(defrel/staged (<=o n m)
  (conde
    ((== n m))
    ((<o n m))))

(defrel/staged (/o n m q r)
  (conde
    ((== r n) (== '() q) (<o n m))
    ((== '(1) q) (=lo n m) (pluso r m n)
     (<o r m))
    ((<lo m n)
     (<o r m)
     (poso q)
     (fresh (nh nl qh ql qlm qlmr rr rh)
       (splito n r nl nh)
       (splito q r ql qh)
       (conde
         ((== '() nh)
          (== '() qh)
          (minuso nl r qlm)
          (*o ql m qlm))
         ((poso nh)
          (*o ql m qlm)
          (pluso qlm r qlmr)
          (minuso qlmr nl rr)
          (splito rr r '() rh)
          (/o nh m qh rh)))))))

(defrel/staged (splito n r l h)
  (conde
    ((== '() n) (== '() h) (== '() l))
    ((fresh (b n^)
       (== `(0 ,b . ,n^) n)
       (== '() r)
       (== `(,b . ,n^) h)
       (== '() l)))
    ((fresh (n^)
       (== `(1 . ,n^) n)
       (== '() r)
       (== n^ h)
       (== '(1) l)))
    ((fresh (b n^ a r^)
       (== `(0 ,b . ,n^) n)
       (== `(,a . ,r^) r)
       (== '() l)
       (splito `(,b . ,n^) r^ '() h)))
    ((fresh (n^ a r^)
       (== `(1 . ,n^) n)
       (== `(,a . ,r^) r)
       (== '(1) l)
       (splito n^ r^ '() h)))
    ((fresh (b n^ a r^ l^)
       (== `(,b . ,n^) n)
       (== `(,a . ,r^) r)
       (== `(,b . ,l^) l)
       (poso l^)
       (splito n^ r^ l^ h)))))

(defrel/staged (logo n b q r)
  (conde
    ((== '(1) n) (poso b) (== '() q) (== '() r))
    ((== '() q) (<o n b) (pluso r '(1) n))
    ((== '(1) q) (>1o b) (=lo n b) (pluso r b n))
    ((== '(1) b) (poso q) (pluso r '(1) n))
    ((== '() b) (poso q) (== r n))
    ((== '(0 1) b)
     (fresh (a ad dd)
       (poso dd)
       (== `(,a ,ad . ,dd) n)
       (exp2 n '() q)
       (fresh (s)
         (splito n dd r s))))
    ((fresh (a ad add ddd)
       (conde
         ((== '(1 1) b))
         ((== `(,a ,ad ,add . ,ddd) b))))
     (<lo b n)
     (fresh (bw1 bw nw nw1 ql1 ql s)
       (exp2 b '() bw1)
       (pluso bw1 '(1) bw)
       (<lo q n)
       (fresh (q1 bwq1)
         (pluso q '(1) q1)
         (*o bw q1 bwq1)
         (<o nw1 bwq1))
       (exp2 n '() nw1)
       (pluso nw1 '(1) nw)
       (/o nw bw ql1 s)
       (pluso ql '(1) ql1)
       (<=lo ql q)
       (fresh (bql qh s qdh qd)
         (repeated-mul b ql bql)
         (/o nw bw1 qh s)
         (pluso ql qdh qh)
         (pluso ql qd q)
         (<=o qd qdh)
         (fresh (bqd bq1 bq)
           (repeated-mul b qd bqd)
           (*o bql bqd bq)
           (*o b bq bq1)
           (pluso bq r n)
           (<o n bq1)))))))

(defrel/staged (exp2 n b q)
  (conde
    ((== '(1) n) (== '() q))
    ((>1o n) (== '(1) q)
     (fresh (s)
       (splito n b s '(1))))
    ((fresh (q1 b2)
       (== `(0 . ,q1) q)
       (poso q1)
       (<lo b n)
       (appendo b `(1 . ,b) b2)
       (exp2 n b2 q1)))
    ((fresh (q1 nh b2 s)
       (== `(1 . ,q1) q)
       (poso q1)
       (poso nh)
       (splito n b s nh)
       (appendo b `(1 . ,b) b2)
       (exp2 nh b2 q1)))))

(defrel/staged (repeated-mul n q nq)
  (conde
    ((poso n) (== '() q) (== '(1) nq))
    ((== '(1) q) (== n nq))
    ((>1o q)
     (fresh (q1 nq1)
       (pluso q1 '(1) q)
       (repeated-mul n q1 nq1)
       (*o nq1 n nq)))))

(defrel/staged (expo b q n)
  (logo n b q '()))

(defrel/staged (pow n q nq)
  (conde
    ((== '(1) q) (== n nq))
    ((>1o q)
     (fresh (q1 nq1)
       (minuso q '(1) q1)
       (repeated-mul n q1 nq1)
       (*o nq1 n nq)))))

;; let rec power (n : int) : int code -> int code =
;;   fun x ->
;;     if n = 0 then .<1>.
;;     else .<.~x * .~(power (n - 1) x)>.;;
;; 
;; .<fun x -> .~((power 5) .<x>.)>.;;
;; (* fun x_1 -> x_1 * (x_1 * (x_1 * (x_1 * (x_1 * 1)))) *)
(defrel/staged (pow/staged n q nq)
  (fallback
   (conde
     ((== '(1) q) (== n nq))
     ((>1o q)
      (fresh (q1 nq1)
        (minuso q '(1) q1)
        (pow/staged n q1 nq1)
        (later (*o nq1 n nq)))))))

(define num2 (build-num 2))
(define num3 (build-num 3))
(define num4 (build-num 4))
(define num8 (build-num 8))
(define num6561 (build-num 6561))


(defrel (pow4-staged n nq)
  (staged
   (pow/staged n num4 nq)))

(defrel (pow4-unstaged n nq)
  (pow/staged n num4 nq))


(test
 ;;#:times 1000
 (run 1 (q1 q2) (pow4-staged num2 q1) (pow4-staged num3 q2))
 '(((0 0 0 0 1) (1 0 0 0 1 0 1))))


(test
 ;;#:times 1000
 (run 1 (q1 q2) (pow4-unstaged num2 q1) (pow4-unstaged num3 q2))
 '(((0 0 0 0 1) (1 0 0 0 1 0 1))))

(defrel/staged (eveno n)
  (fresh (rest)
    (== n `[0 . ,rest])))

;; let rec power2 (n : int) : int code -> int code =
;;   fun x ->
;;     if n = 0 then
;;       .<1>.
;;     else if n mod 2 = 0 then
;;       .< (let v = .~(power2 (n / 2) x) in v * v) >.
;;     else
;;       .< .~x * .~(power2 (n - 1) x) >.;;
;; .<fun x -> .~((power2 5) .<x>.)>.;;
;; (* fun x_2 -> x_2 * (let v_4 = (let v_3 = (x_2 * 1) in v_3 * v_3) in v_4 * v_4) *)
(defrel/staged (pow/staged2 n q nq)
  (fallback
   (conde
     ((== '(1) q) (== n nq))
     ((>1o q)
      (conde
        [(fresh (q/2 nq1)
           (== q `(0 . ,q/2))
           (pow/staged2 n q/2 nq1)
           (later (*o nq1 nq1 nq)))]
        [(fresh (rest)
           (== q `(1 . ,rest))
           (fresh (q1 nq1)
             (minuso q '(1) q1)
             (pow/staged2 n q1 nq1)
             (later (*o nq1 n nq))))]
      )))))


(defrel (pow4/2 n nq)
  (staged
   (pow/staged2 n num4 nq)))


(test
 ;; #:times 1000
 (run 1 (q1 q2) (pow4/2 num2 q1) (pow4/2 num3 q2))
 '(((0 0 0 0 1) (1 0 0 0 1 0 1))))

(defrel (pow4/2-unstaged n nq)
  (pow/staged2 n num4 nq))


(test
 ;;#:times 1000
 (run 1 (q1 q2) (pow4/2-unstaged num2 q1) (pow4/2-unstaged num3 q2))
 '(((0 0 0 0 1) (1 0 0 0 1 0 1))))

;; let rec power3 (n : int) : int code -> int code =
;;   fun x ->
;;     if n = 0 then
;;       .<1>.
;;     else if n mod 2 = 0 then
;;       power3 (n / 2) .<let v = .~x in v * v>.
;;     else
;;       .< .~x * .~(power3 (n - 1) x) >.;;
;; .<fun x -> .~((power3 5) .<x>.)>.;;
;; (* fun x_8 -> x_8 * ((let v_10 = (let v_9 = x_8 in v_9 * v_9) in v_10 * v_10) * 1) *)
(defrel/staged (pow/staged3 n q nq)
  (fallback
   (conde
     ((== '(1) q) (== n nq))
     ((>1o q)
      (conde
        [(fresh (q/2 nq1)
           (== q `(0 . ,q/2))
           (later (*o n n nq1))
           (pow/staged3 nq1 q/2 nq))]
        [(fresh (rest)
           (== q `(1 . rest))
           (fresh (q1 nq1)
             (minuso q '(1) q1)
             (pow/staged3 n q1 nq1)
             (later (*o nq1 n nq))))]
      )))))


(defrel (pow3/3 n nq)
  (staged
   (pow/staged3 n num3 nq)))


(test
 ;;#:times 1000
 (run 1 (q1) (pow3/3 num2 q1))
 '((0 0 0 1)))

(defrel (pow4/3 n nq)
  (staged
   (pow/staged3 n num4 nq)))


(test
 ;;#:times 1000
 (run 1 (q1 q2) (pow4/3 num2 q1) (pow4/3 num3 q2))
 '(((0 0 0 0 1) (1 0 0 0 1 0 1))))

(defrel (pow4/3-unstaged n nq)
  (pow/staged3 n num4 nq))


(test
 ;;#:times 1000
 (run 1 (q1 q2) (pow4/3-unstaged num2 q1) (pow4/3-unstaged num3 q2))
 '(((0 0 0 0 1) (1 0 0 0 1 0 1))))


(record-bench 'simple 'staging 'pow8-backward)
(defrel (pow8 n nq)
  (time-staged
   (pow/staged3 n num8 nq)))




(test
 ;;#:times 1000
 (run 1 (q2) (pow8 num3 q2))
 '((1 0 0 0 0 1 0 1 1 0 0 1 1)))

(defrel (pow8-unstaged n nq)
  (pow/staged3 n num8 nq))


(test
 ;;#:times 1000
 (run 1 (q2) (pow8-unstaged num3 q2))
 '((1 0 0 0 0 1 0 1 1 0 0 1 1)))



(record-bench 'simple 'staged 'pow8-backward)
(time-test
 #:times 1000
 (run 1 (q2) (pow8 q2 num6561))
 '((1 1)))

(record-bench 'simple 'unstaged 'pow8-backward #:description "Solve $n^8=6561$ via relational arithmetic~\\cite{DBLP:conf/flops/KiselyovBFS08} (x1000)")
(time-test
 #:times 1000
 (run 1 (q2) (pow8-unstaged q2 num6561))
 '((1 1)))
