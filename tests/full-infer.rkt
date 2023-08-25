#lang racket/base

;; from
;; https://github.com/webyrd/normalization-by-evaluation/blob/master/miniKanren-version/naive/nbe-untagged-extended-infer.scm
;; https://github.com/webyrd/normalization-by-evaluation/blob/master/miniKanren-version/naive/nbe-untagged-extended-infer-tests.scm

(require racket/pretty
         "../main.rkt"
         "../test-check.rkt")

(defrel/multistage/fallback (lookupo x env val)
  (fresh (y v env^)
    (== `((,y . ,v) . ,env^) env)
    (symbolo x)
    (symbolo y)
    (conde
      ((== x y) (== v val))
      ((=/= x y)
       (lookupo x env^ val)))))

(defrel/multistage/fallback (!-o gamma expr type)
  (conde
    ((== #f expr) (== 'Bool type))
    ((== #t expr) (== 'Bool type))
    ((numbero expr) (== 'Nat type))
    ((fresh (datum)
       (== `(quote ,datum) expr)
       (absento 'closure datum)
       (absento 'N datum)
       (!-quoted-datumo datum type)))
    ((symbolo expr) (lookupo expr gamma type))
    ((fresh (e)
       (== `(Ann ,e ,type) expr)
       (checko gamma e type)))
    ((fresh (e t)
       (== `(null? ,e) expr)
       (== 'Bool type)
       (!-o gamma e t)))
    ((fresh (e t)
       (== `(pair? ,e) expr)
       (== 'Bool type)
       (!-o gamma e t)))
    ((fresh (e t)
       (== `(number? ,e) expr)
       (== 'Bool type)
       (!-o gamma e t)))
    ((fresh (e t)
       (== `(symbol? ,e) expr)
       (== 'Bool type)
       (!-o gamma e t)))
    ((fresh (e)
       (== `(car ,e) expr)
       (!-o gamma e `(List ,type))))
    ((fresh (e a)
       (== `(cdr ,e) expr)
       (== `(List ,a) type)
       (!-o gamma e `(List ,a))))
    ((fresh (e1 e2 a)
       (== `(cons ,e1 ,e2) expr)
       (== `(List ,a) type)
       (!-o gamma e2 `(List ,a))
       (!-o gamma e1 a)))
    ((fresh (e1 e2 e3)
       (== `(if ,e1 ,e2 ,e3) expr)
       (checko gamma e1 'Bool)
       (!-o gamma e2 type)
       (checko gamma e3 type)))
    ((fresh (e1 e2 t1)
       (== `(,e1 ,e2) expr)
       (!-o gamma e1 `(-> ,t1 ,type))
       (checko gamma e2 t1)))))

(defrel/multistage/fallback (!-quoted-datumo datum type)
  (conde
    ((== #f datum) (== 'Bool type))
    ((== #t datum) (== 'Bool type))
    ((numbero datum) (== 'Nat type))
    ((symbolo datum) (== 'Sym type))
    ((== '() datum)
     (fresh (a)
       (== `(List ,a) type)))
    ((fresh (v1 v2 a)
       (== `(,v1 . ,v2) datum)
       (== `(List ,a) type)
       (!-quoted-datumo v2 `(List ,a))
       (!-quoted-datumo v1 a)))))

(defrel/multistage/fallback (checko gamma expr type)
  (conde
    ((fresh (x body t1 t2)
       (== `(lambda (,x) ,body) expr)
       (== `(-> ,t1 ,t2) type)
       (symbolo x)
       (checko `((,x . ,t1) . ,gamma) body t2)))
    ((!-o gamma expr type))))

(test ;;"infer-if-1"
  (run* (q) (!-o '() '(if (null? (quote (5 6))) #f #t) q))
  '(Bool))

(test ;;"infer-if-2"
  (run* (q) (!-o '() '(if (null? (quote (5 6))) #f 5) q))
  '())

(test ;;"infer-if-3"
  (run* (q)
    (!-o '()
         '(if (null? (quote (5 6)))
              (cons 3 (quote ()))
              (cons 4 (quote ())))
         q))
  '((List Nat)))

(test ;;"infer-if-4"
  (run* (q) (!-o '() '(if 5 #f #t) q))
  '())


(test ;;"infer-Bool-1"
  (run* (q) (!-o '() '#f q))
  '(Bool))

(test ;;"infer-Bool-2"
  (run* (q) (!-o '() '#t q))
  '(Bool))

(test ;;"infer-Nat-2"
  (run* (q) (!-o '() '5 q))
  '(Nat))

(test ;;"infer-var-1"
  (run* (q) (!-o '() 'x q))
  '())

(test ;;"infer-Ann-1"
  (run* (q) (!-o '() '(Ann (lambda (x) x) (-> Nat Nat)) q))
  '((-> Nat Nat)))

(test ;;"infer-Ann-2"
  (run* (q) (!-o '() '(Ann (lambda (x) 5) (-> Bool Nat)) q))
  '((-> Bool Nat)))

(test ;;"infer-Ann-3"
  (run* (q) (!-o '() '(Ann (lambda (x) 5) (-> Nat Nat)) q))
  '((-> Nat Nat)))

(test ;;"infer-Ann-4"
  (run* (q) (!-o '() '(Ann (lambda (x) (car x)) (-> Nat Nat)) q))
  '())

(test ;;"infer-Ann-5"
  (run* (q) (!-o '() '(Ann (lambda (x) (car x)) (-> (List Nat) Nat)) q))
  '((-> (List Nat) Nat)))

(test ;;"infer-Ann-6"
  (run* (q) (!-o '() '(Ann (lambda (x) (cons 3 x)) (-> (List Nat) (List Nat))) q))
  '((-> (List Nat) (List Nat))))

(test ;;"infer-Ann-7"
  (run* (q)
    (!-o '()
         '(Ann (lambda (x) (lambda (y) (cons x y)))
               (-> Nat (-> (List Nat) (List Nat))))
         q))
  '((-> Nat (-> (List Nat) (List Nat)))))

(test ;;"infer-Ann-8"
  (run* (q)
    (!-o '()
         '(Ann (lambda (f) (f 5))
               (-> (-> Nat Bool) Bool))
         q))
  '((-> (-> Nat Bool) Bool)))

(test ;;"infer-Ann-9"
  (run* (q)
    (!-o '()
         '(Ann (lambda (n) (lambda (f) (f n)))
               (-> Nat (-> (-> Nat Bool) Bool)))
         q))
  '((-> Nat (-> (-> Nat Bool) Bool))))

(test ;;"infer-Ann-10"
  (run* (q)
    (!-o '()
         '((Ann (lambda (n) (lambda (f) (f n)))
                (-> Nat (-> (-> Nat Bool) Bool)))
           
           5)
         q))
  '((-> (-> Nat Bool) Bool)))

(test ;;"infer-Ann-11"
  (run* (q)
    (!-o '()
         '((Ann (lambda (f) (lambda (n) (f n)))
                (-> (-> Nat Bool) (-> Nat Bool)))
           (Ann (lambda (y) #f)
                (-> Nat Bool)))
         q))
  '((-> Nat Bool)))

(test ;;"infer-Ann-12"
  (run* (q)
    (!-o '()
         '(((Ann (lambda (f) (lambda (n) (f n)))
                 (-> (-> Nat Bool) (-> Nat Bool)))
            (Ann (lambda (y) #f)
                 (-> Nat Bool)))
           5)
         q))
  '(Bool))


(test ;;"infer-quote-1"
  (run* (q) (!-o '() '(quote ()) q))
  '((List _.0)))

(test ;;"infer-quote-2"
  (run* (q) (!-o '() '(quote 5) q))
  '(Nat))

(test ;;"infer-quote-3"
  (run* (q) (!-o '() '(quote (5 6)) q))
  '((List Nat)))

(test ;;"infer-quote-4"
  (run* (q) (!-o '() '(quote (#t #f)) q))
  '((List Bool)))

(test ;;"infer-quote-5"
  (run* (q) (!-o '() '(quote (5 #f)) q))
  '())

(test ;;"infer-quote-6"
  (run* (q) (!-o '() '(quote ((#t) (#f))) q))
  '((List (List Bool))))

(test ;;"infer-quote-7"
  (run* (q) (!-o '() '(quote ((cat) (dog))) q))
  '((List (List Sym))))


(test ;;"infer-null?-1"
  (run* (q) (!-o '() '(null? (quote ())) q))
  '(Bool))

(test ;;"infer-pair?-1"
  (run* (q) (!-o '() '(pair? (quote ())) q))
  '(Bool))

(test ;;"infer-number?-1"
  (run* (q) (!-o '() '(number? (quote ())) q))
  '(Bool))

(test ;;"infer-symbol?-1"
  (run* (q) (!-o '() '(symbol? (quote ())) q))
  '(Bool))

(test ;;"infer-list-1"
  (run* (q) (!-o '() '(cons 5 (quote ())) q))
  '((List Nat)))

(test ;;"infer-list-2"
  (run* (q) (!-o '() '(cons 5 (cons 6 (quote ()))) q))
  '((List Nat)))

(test ;;"infer-list-3"
  (run* (q) (!-o '() '(cons #f (cons #t (quote ()))) q))
  '((List Bool)))

(test ;;"infer-list-4"
  (run* (q) (!-o '() '(cons 5 (cons #t (quote ()))) q))
  '())

(test ;;"infer-list-5"
  (run* (q) (!-o '() '(cons (cons 5 (quote ())) (cons (cons 6 (quote ())) (quote ()))) q))
  '((List (List Nat))))

(test ;;"infer-car-1"
  (run* (q) (!-o '() '(car (cons (cons 5 (quote ())) (cons (cons 6 (quote ())) (quote ())))) q))
  '((List Nat)))

(test ;;"infer-car-2"
  (run* (q) (!-o '() '(car (quote ())) q))
  '(_.0))

(test ;;"infer-car-3"
  (run* (q) (!-o '() '(car (quote 5)) q))
  '())

(test ;;"infer-car-4"
  (run* (q) (!-o '() '(car (quote (5 6))) q))
  '(Nat))

(test ;;"infer-cdr-1"
  (run* (q) (!-o '() '(cdr (cons (cons 5 (quote ())) (cons (cons 6 (quote ())) (quote ())))) q))
  '((List (List Nat))))

(test ;;"infer-cdr-2"
  (run* (q) (!-o '() '(cdr (quote ())) q))
  '((List _.0)))

(test ;;"infer-cdr-3"
  (run* (q) (!-o '() '(cdr (quote 5)) q))
  '())

(test ;;"infer-cdr-4"
  (run* (q) (!-o '() '(cdr (quote (5 6))) q))
  '((List Nat)))
