;; # Utilities for quick-fixing scope
(define union
  (lambda (a b)
    (if (null? a) b
        (union (cdr a) (if (memq (car a) b) b (cons (car a) b))))))

(define diff
  (lambda (a b)
    (if (null? b) a (remq (car b) (diff a (cdr b))))))

#|
goals :=
(== term term)
(fresh (x ...) goal...)
(conde (goal ...) ...)
term :=
(cons term term)
(lambda x goal)
(lambda (x ...) goal)
(quote datum)
(list term...)
(data var)

things to manipulate:
reified logic variables, always in data position
fresh syntax that is not quoted

by the way we construct lambdas in lreify-call, the parameter names do not intersect with logic variables, so we don't need to consider lambda especially in fix scope.

|#

#|
for each later state variable, fix-scope1-syntax will add a binding to the variable to the immediately surrounding fresh.
fix-scope2-syntax keeps only the outermost fresh binding for a variable.
|#
(define fix-scope1-syntax
  (syntax-parser
   #:literals (fresh quote)
   [(fresh (x ...+) goal ...)
    (error 'fix-scope1-syntax "encountered fresh with variables")]
   [(fresh () goal ...)
    (define r (map fix-scope1-syntax (attribute goal)))
    (define/syntax-parse (goal^ ...) (map first r))
    (define/syntax-parse (var ...) (apply set-union (map second r)))
    (list #'(fresh (var ...) goal^ ...) (list))]
   [(quote datum)
    (list this-syntax (list))]
   [d
    #:when (data? (syntax-e #'d))
    (define var (data-value (syntax-e #'d)))
    (list #`#,var (list var))]
   [(a . d)
    (define ra (fix-scope1-syntax #'a))
    (define rd (fix-scope1-syntax #'d))
    (list #`(#,(first ra) . #,(first rd)) (set-union (second ra) (second rd)))]
   [_ (list this-syntax (list))]))

(define fix-scope2-syntax
  (lambda (stx bound-vars)
    (syntax-parse
     stx
     #:literals (fresh quote)
     [(fresh (x ...) goal ...)
      (define xs (map syntax-e (attribute x)))
      (define/syntax-parse (x^ ...) (set-subtract xs bound-vars))
      (define bound-vars^ (set-union xs bound-vars))
      (define/syntax-parse (goal^ ...)
        (for/list ([g (attribute goal)]) (fix-scope2-syntax g bound-vars^)))
      #'(fresh (x^ ...) goal^ ...)]
     [(quote datum)
      this-syntax]
     [(a . d)
      #`(#,(fix-scope2-syntax #'a bound-vars) . #,(fix-scope2-syntax #'d bound-vars))]
     [_ this-syntax])))

(define fix-scope-syntax
  (lambda (stx)
    (define r (fix-scope1-syntax stx))
    (unless (null? (second r))
      (error 'fix-scope-syntax "unscoped variable"))
    (fix-scope2-syntax (first r) '())))

(define fix-scope1
  (lambda (t . in-cdr)
    (cond
      ((symbol? t)
       (list t (if (reified-var? t) (list t) (list))))
      ((and (null? in-cdr) (pair? t) (eq? 'fresh (car t)))
       (let ((r (map fix-scope1 (cddr t))))
         (let ((body (map car r))
               (vs (fold-right union
			       (filter (lambda (x) (not (reified-var? x))) (cadr t))
			       (map cadr r))))
           (list `(fresh ,vs . ,body) (list)))))
      ((and (pair? t) (eq? 'lambda (car t)) (not (null? (cdr t))))
       (let ((r (map fix-scope1 (cddr t))))
         (let ((body (map car r))
               (vs (diff (fold-right union '() (map cadr r))
                         (if (symbol? (cadr t)) (list (cadr t)) (cadr t)))))
           (list `(lambda ,(cadr t) . ,body) vs))))
      ((pair? t)
       (let ((ra (fix-scope1 (car t)))
             (rb (fix-scope1 (cdr t) #t)))
         (list (cons (car ra) (car rb)) (union (cadr ra) (cadr rb)))))
      (else (list t (list))))))

(define fix-scope2
  (lambda (t s . in-cdr)
    (cond
      ((and (null? in-cdr) (pair? t) (eq? 'fresh (car t)))
       (let ((ds (diff (cadr t) (filter reified-var? s)))
             (us (union (cadr t) s)))
         `(fresh ,ds . ,(map (lambda (x) (fix-scope2 x us)) (cddr t)))))
      ((and (pair? t) (eq? 'lambda (car t)) (not (null? (cdr t))))
       (let ((us (union (if (symbol? (cadr t)) (list (cadr t)) (cadr t)) s)))
         `(lambda ,(cadr t) . ,(map (lambda (x) (fix-scope2 x us)) (cddr t)))))
      ((pair? t)
       (cons (fix-scope2 (car t) s) (fix-scope2 (cdr t) s #t)))
      (else t))))

(define fix-scope
  (lambda (t)
    (car (fix-scope2 (fix-scope1 t) '()))))

(define (unique-result r)
  (cond
    ((null? r)
     (error 'gen "staging failed"))
    ((not (null? (cdr r)))
     (for-each
       (lambda (i x) (printf "result ~a: ~a\n" (+ 1 i) x))
       (iota (length r))
       r)
     (error 'gen "staging non-deterministic"))
    (else (car r))))
(define (layer? tag r)
  (and (pair? r) (pair? (cdr r)) (eq? tag (cadr r))))
(define (code-layer? r)
  (layer? '!! r))
(define (constraint-layer? r)
  (layer? '$$ r))
(define (maybe-remove-constraints r)
  (if (constraint-layer? r)
      (car r)
      r))
(define (convert-constraints r)
  (cond
    ((constraint-layer? r)
     (printf "processing constraints: ~a\n" (cddr r))
     (process-constraints (cddr r)))
    (else '())))
(define (process-constraints cs)
  (cond
    ((null? cs) '())
    (else (append (process-constraint (car cs))
                  (process-constraints (cdr cs))))))
(define (process-constraint c)
  (printf "processing constraint: ~a\n" c)
  (cond
    ((eq? (car c) '=/=)
     (map (lambda (x) #`(=/= #,(miniexpand (caar x)) #,(miniexpand (cadar x))))
          (cdr c)))
    ((eq? (car c) 'absento)
     (map (lambda (x) #`(absento #,(miniexpand (car x)) #,(miniexpand (cadr x))))
          (cdr c)))
    ((eq? (car c) 'sym)
     (map (lambda (x) #`(symbolo #,x)) (cdr c)))
    ((eq? (car c) 'num)
     (map (lambda (x) #`(numbero #,x)) (cdr c)))
    (else (error 'process-constraint (format "unexpected constraint: ~a" c)))))

(define (miniexpand x)
  (cond
    ((reified-var? x) x)
    (else #`(quote #,x))))
(define (reified-expand x)
  (cond
    ((reified-var? x) (data x))
    ((pair? x)
     #`(cons
        #,(reified-expand (car x))
        #,(reified-expand (cdr x))))
    (else #`(quote #,x))))
;; # Helpers for turning functional procedure into relational one
(define res #f)

(define (strip-data-from-syntax x)
  (map-on-syntax-data (lambda (v) v) x))

(define (to-datum x)
  (map syntax->datum (map strip-data-from-syntax x)))

(define (gen-func r . inputs)
  (let ((r (unique-result r)))
      (let ((cs (convert-constraints r))
            (r (maybe-remove-constraints r)))
        (unless (code-layer? r)
          (error 'gen-func (format "no code generated: ~a" r)))
        (set! res
          (fix-scope-syntax
            #`(lambda (#,@inputs)
                (fresh () #,@cs (== #,(reified-expand (car r)) (list #,@inputs)) . #,(caddr r)))))
        res)))

(define-syntax-rule (generate-staged (var ...) goal)
  (eval-syntax
   (gen-func
    (run 100 (var ... bogus-var1 bogus-var2) goal)
    'var ... 'bogus-var1 'bogus-var2)))

(define (invoke-staged f . args)
  (apply f (append args (list 'bogus-1 'bogus-2))))

(define (generated-code)
  (and res (syntax->datum res)))

(define (reset-generated-code!)
  (set! res #f))
