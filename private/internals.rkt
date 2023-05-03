#lang racket/base

(provide (all-defined-out))

(require racket/list
         racket/include
         racket/match
         racket/set
         syntax/parse
         (for-syntax racket/base syntax/parse))

(include "../../faster-minikanren/racket-compatibility.scm")
(include "../../faster-minikanren/mk.scm")


(define (map-on-syntax-data f stx)
  (let rec ((v stx))
    (cond
      ((syntax? v)
       (datum->syntax v (rec (syntax-e v)) v v))
      ((pair? v)
       (cons (rec (car v)) (rec (cdr v))))
      ((data? v)
       (f (data-value v)))
      (else v))))

;; called from mk.scm `reify-S`
(define (reify-S-syntax v S)
  (cond
    ((syntax? v)
     (reify-S-syntax (syntax-e v) S))
    ((pair? v)
     (let ((S (reify-S-syntax (car v) S)))
       (reify-S-syntax (cdr v) S)))
    ((data? v)
     (reify-S (data-value v) S))
    (else S)))

(define (do-expand t)
  (map-on-syntax-data reflect-datum t))

(define (unexpand x) x)
(define (unexpand? x) (syntax? x))

;; used in mk.scm `vars`
(struct data [value] #:transparent)


(define (expand x) (data x))
(define (expand? x) (data? x))

(define (reflect-datum t)
  (define (nonliteral-datum? t)
    (or (var? t) (syntax? t) (apply-rep? t)))
  
  (define (needs-unquote? t)
    (cond
      ((nonliteral-datum? t) #t)
      ((pair? t) (or (needs-unquote? (car t)) (needs-unquote? (cdr t))))
      (else #f)))

  (define (reflect-nonliteral-datum t)
    (match t
      [(? var?) (data t)]
      [(? syntax? t) (do-expand t)]
      [(apply-rep name-staged name-dyn args proc)
       #`(make-apply-rep
          #,(reflect-datum name-staged)
          #,(reflect-datum name-dyn)
          #,(reflect-datum args)
          #,(reflect-datum proc))]))
  
  (define (reflect-quasi-contents t)
    (match t
      [(? nonliteral-datum?) #`,#,(reflect-nonliteral-datum t)]
      [(cons a d)
       (cons (reflect-quasi-contents a) (reflect-quasi-contents d))]
      [t t]))

  (cond
    [(nonliteral-datum? t)
     (reflect-nonliteral-datum t)]
    [(needs-unquote? t)
     #``#,(reflect-quasi-contents t)]
    [else
     #`'#,(reflect-quasi-contents t)]))

;; called from mk.scm `walk*`
(define (walk*-syntax stx S)
  (map-on-syntax-data (lambda (v) (data (walk* v S))) stx))

;; called from mk.scm `reify`
(define walk-later-final
  (lambda (L S)
    (map do-expand (walk-later L S))))

(define walk-later
  (lambda (L S)
    (map (lambda (stx) (walk* stx S)) (reverse L))))

(define later
  (lambda (x)
    (lambda (st)
      (state (state-S st) (state-C st) (cons x (state-L st))))))

(define later-scope
  (lambda (g out)
    (lambda (st)
      (bind*
       (g (state (state-S st) (C-new-later-scope (state-C st)) '()))
       generate-constraints
       (lambda (st2)
         ((fresh ()
            (== out (walk-later (state-L st2) (state-S st2))))
          st))))))

(define capture-later
  (lambda (g k)
    (lambda (st)
      (bind*
       (g (state-with-scope
           (state (state-S st) (C-new-later-scope (state-C st)) '())
           (new-scope)))
       generate-constraints
       (lambda (st2)
         ((k (walk-later (state-L st2) (state-S st2)))
          st))))))

(define (ldisj . gs-init)
  (let recur ([gs gs-init] [gs-stx '()])
    (if (null? gs)
        (later #`(conde #,@(reverse gs-stx)))
        (capture-later (car gs)
                       (lambda (g-stx)
                         (recur (cdr gs) (cons g-stx gs-stx)))))))

(define-syntax lconde
  (syntax-rules ()
    ((_ (g ...) ...)
     (ldisj (fresh () g ...) ...))))

;; TODO: this relies on internal details, only works for current set of type constraints.
;;  Should figure how to make generic in type constraints at least.
(define (generate-var-constraints st)
  (lambda (v)
    (let ([c (lookup-c st v)])
      (if (eq? c empty-c)
          st
          (append
           (if (c-T c)
               (let ((cid (hash-ref 
                           (hasheq 'sym #'symbolo 'num #'numbero 'str #'stringo)
                           (type-constraint-reified (c-T c)))))
                 (list #`(#,cid #,(expand v))))
               '())
           (map (lambda (atom) #`(absento #,(expand atom) #,v)) (c-A c))
           (map (lambda (d) #`(=/=* #,(expand d))) (c-D c)))))))

(define generate-constraints
  (lambda (st)
    (let* ([vars (remove-duplicates (C-vars (state-C st)))]
           [new-stx (apply append (map (generate-var-constraints st) vars))])
      (state (state-S st) (state-C st) (append (state-L st) (reverse new-stx))))))

(define (later-binary-constraint constraint-id)
  (lambda (t1 t2) (later #`(#,constraint-id #,(expand t1) #,(expand t2)))))

(define (later-unary-constraint constraint-id)
  (lambda (t) (later #`(#,constraint-id #,(expand t)))))

(define-values (l== l=/= labsento)
  (apply values (map later-binary-constraint (list #'== #'=/= #'absento))))

(define-values (lsymbolo lnumbero lstringo)
  (apply values (map later-unary-constraint (list #'symbolo #'numbero #'stringo))))

(define-syntax lapp
  (syntax-rules ()
    [(_ relation arg ...)
     (later #`(relation #,(expand arg) ...))]))

(define lfail (later #'fail))
(define lsucceed (later #'succeed))


;; used in mk.scm `unify` and `walk*`
(struct apply-rep [name-staged name-dyn args proc]
  #:prefab
  #:constructor-name make-apply-rep)


(define-syntax lreify-call
  (lambda (stx)
    (syntax-case stx ()
        ((_ rep ((rel-staged rel-dyn) (x ...) (y ...)))
         (andmap (lambda (id) (free-identifier=? id #'_)) (syntax->list #'(y ...)))
         (with-syntax
          (((y-n ...) (generate-temporaries #'(y ...)))
           ((y-n2 ...) (generate-temporaries #'(y ...))))
          #'(fresh (y-n ...)
              (capture-later
               (fresh ()
                 (l== y-n (unexpand #'y-n2)) ...
                 (rel-staged rep x ... y-n ...))
               (lambda (body)
                 (l== rep (make-apply-rep
                           'rel-staged 'rel-dyn (list x ...)
                           (unexpand #`(lambda (y-n2 ...)
                                        (fresh ()
                                          . #,body)))))))))))))

(define-syntax reify-call
  (lambda (stx)
    (syntax-case stx ()
        ((_ rep ((rel-staged rel-dyn) (x ...) (y ...)))
         (andmap (lambda (id) (free-identifier=? id #'_)) (syntax->list #'(y ...)))
         #'(== rep (make-apply-rep
                    'rel-staged 'rel-dyn (list x ...)
                    #f))))))

(define-syntax apply-reified
  (lambda (stx)
    (syntax-case stx ()
        ((_ rep ((rel-staged rel-dyn) (x ...) (y ...)))
         (andmap (lambda (id) (free-identifier=? id #'_)) (syntax->list #'(x ...)))
         (with-syntax
          (((x-n ...) (generate-temporaries #'(x ...))))
          #'(lambda (st)
              (let ((rep (walk rep (state-S st))))
                        ;;(printf "project~\n")
                        ((cond
                           ((var? rep)
                            (fresh (x-n ...)
                                   (== rep (make-apply-rep
                                             'rel-staged 'rel-dyn (list x-n ...) ;
                                             #f))
                                   (rel-dyn x-n ... y ...)))
                           ((apply-rep? rep)
                            ;;(printf "applying rep...~\n")
                            (let ((proc (apply-rep-proc rep)))
                              ;; TODO: unify to check names
                              (if (or (not proc) (unexpand? proc))
                                (apply rel-dyn (append (apply-rep-args rep) (list y ...)))
                                (begin
                                  ;;(printf "calling proc...~\n")
                                  (proc y ...)))))
                           (else fail)) st))))))))

(define-syntax lapply-reified
  (syntax-rules ()
    ((_ rep ((rel-staged rel-dyn) (x ...) (y ...)))
     (later #`(apply-reified #,(expand rep) ((rel-staged rel-dyn) (x ...) (#,(expand y) ...)))))))



(define (evaluate-guard thunk-stream guard-stx)
  (match (take 2 thunk-stream)
    ['() #f]
    [(list answer) answer]
    [answers
     'nondet
     ;;(raise-syntax-error 'condg (format "guard produced too many answers: ~a" answers) guard-stx)
     ]))

(define (condg-runtime fallback clauses error-stx)
  (lambda (st)
    (let ((st (state-with-scope st (new-scope))))
      (define candidates
        (for/list ([clause clauses]
                   #:do [(match-define (list guard-stream body guard-stx) (clause st))
                         (define guard-answer (evaluate-guard (lambda () guard-stream) guard-stx))]
                   #:when guard-answer)
          (cons guard-answer body)))
      (match candidates
        ['() (raise-syntax-error #f "all guards failed" error-stx)]
        [(list (cons guard-answer body))
         (if (eq? 'nondet guard-answer)
             (fallback st)
             (body guard-answer))]
        [_ (fallback st)]))))

(define-syntax condg
  (syntax-parser
   ((_ fallback ((x ...) (~and guard (g0 g ...)) (b0 b ...)) ...)
    #:with error-stx this-syntax
    #'(condg-runtime
       (lambda (st) (fallback st))
       (list
         (lambda (st)
           (let ((scope (subst-scope (state-S st))))
             (let ((x (var scope)) ...)
               (list
                 (bind* (g0 st) g ...)
                 (lambda (st) (bind* (b0 st) b ...))
                 #'guard))))
         ...)
       #'error-stx))))




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
     (process-constraints (cddr r)))
    (else '())))
(define (process-constraints cs)
  (cond
    ((null? cs) '())
    (else (append (process-constraint (car cs))
                  (process-constraints (cdr cs))))))
(define (process-constraint c)
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

(define-syntax run-staged
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (begin
       (printf "running first stage\n")
       (let* ((f (gen-func
                  (run 100 (q bogus-var) g0 g ...)
                  'out 'bogus-var))
              (e (eval-syntax f)))
         (printf "running second stage\n")
         (run n (q) (e q 'bogus-val)))))
    ((_ n (q0 q1 q ...) g0 g ...)
     (run-staged n (x)
                 (fresh (q0 q1 q ...)
                   g0 g ...
                   (l== `(,q0 ,q1 ,q ...) x))))))

(define-syntax run-staged*
  (syntax-rules ()
    ((_ (q0 q ...) g0 g ...) (run-staged #f (q0 q ...) g0 g ...))))

(define-syntax define-relation
  (syntax-rules ()
    ((_ (name x ...) g0 g ...)
     (define (name x ...)
       (fresh () g0 g ...)))))

(define-syntax define-staged-relation
  (syntax-rules ()
    ((_ (name x0 x ...) g0 g ...)
     (define name (staged-relation (x0 x ...) g0 g ...)))))

(define-syntax staged-relation
  (syntax-rules ()
    [(_ (x0 x ...) g0 g ...)
     (time
      (eval-syntax
       (gen-func
        (run 100 (x0 x ...) g0 g ...)
        'x0 'x ...)))]))
