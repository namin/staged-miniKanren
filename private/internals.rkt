#lang racket/base

(provide (all-defined-out))

(require racket/list
         racket/include
         racket/match
         racket/set
         syntax/parse
         (for-syntax racket/base syntax/parse))

(include "faster-minikanren/racket-compatibility.scm")
(include "faster-minikanren/mk.scm")

;;
;; Extensions to the data type of terms
;;
;;
;; Term := ...
;;       | ApplyRep      ; first-class partial relation application value
;;

;; ApplyRep = (apply-rep Symbol Symbol Term (or #f SyntaxWithData Procedure))
;; The proc is syntax at staging time and a procedure at runtime.
;;
;; called in mk.scm `unify` and `walk*`
(struct apply-rep [name-staged name-dyn args proc] #:prefab)

;; A SyntaxWithData is a syntax object containing (data Term) structures
;;  in some positions.
;;
;; called in mk.scm `vars`
(struct data [value] #:transparent)

(define (map-syntax-with-data f stx)
  (let rec ((v stx))
    (cond
      [(syntax? v)
       (datum->syntax v (rec (syntax-e v)) v v)]
      [(pair? v)
       (cons (rec (car v)) (rec (cdr v)))]
      [(data? v)
       (f (data-value v))]
      [else v])))

;; called from mk.scm `walk*`
(define (walk*-syntax stx S)
  (map-syntax-with-data (lambda (v) (data (walk* v S))) stx))



;;
;; Basic "later" constraint and goal variants
;;

(define (later x)
  (lambda (st)
    (state-with-L st (cons x (state-L st)))))

(define (later-binary-constraint constraint-id)
  (lambda (t1 t2)
    (later #`(#,constraint-id #,(data t1) #,(data t2)))))

(define (later-unary-constraint constraint-id)
  (lambda (t)
    (later #`(#,constraint-id #,(data t)))))

(define-values (l== l=/= labsento)
  (apply values (map later-binary-constraint (list #'== #'=/= #'absento))))

(define-values (lsymbolo lnumbero lstringo)
  (apply values (map later-unary-constraint (list #'symbolo #'numbero #'stringo))))

(define-syntax lapp
  (syntax-rules ()
    [(_ relation arg ...)
     (later #`(relation #,(data arg) ...))]))

(define lsucceed (later #'succeed))
(define lfail (later #'fail))

(define-syntax lconde
  (syntax-rules ()
    ((_ (g ...) ...)
     (ldisj (fresh () g ...) ...))))

(define (ldisj . gs-init)
  (let recur ([gs gs-init] [gs-stx '()])
    (if (null? gs)
        (later #`(conde #,@(reverse gs-stx)))
        (capture-later (car gs)
                       (lambda (g-stx)
                         (recur (cdr gs) (cons g-stx gs-stx)))))))

;;
;; Scoped lift capturing
;;

(define (capture-later g k)
  (lambda (st-original)
    (let* ([st-before (state-with-C st-original (C-new-later-scope (state-C st-original)))]
           [st-before (state-with-L st-before '())]
           [st-before (state-with-scope st-before (new-scope))])
      (bind
       (g st-before)
       (lambda (st-after)
         (let ([captured-L (for/list ([stx (append (reverse (state-L st-after))
                                                   (generate-constraints st-after))])
                             (walk* stx (state-S st-after)))])
           ((k captured-L)
            st-original)))))))


(define (generate-constraints st)
  (let ([vars (remove-duplicates (reverse (C-vars (state-C st))))])
    (apply append (map (generate-var-constraints st) vars))))

;; TODO: this relies on internal details, only works for current set of type constraints.
;;  Should figure how to make generic in type constraints at least.
(define (generate-var-constraints st)
  (lambda (v)
    (let ([c (lookup-c st v)])
      (if (eq? c empty-c)
          st
          (append
           (if (c-T c)
               (let ((cid (hash-ref (hasheq 'sym #'symbolo 'num #'numbero 'str #'stringo)
                                    (type-constraint-reified (c-T c)))))
                 (list #`(#,cid #,(data v))))
               '())
           (map (lambda (atom) #`(absento #,(data atom) #,v)) (c-A c))
           (map (lambda (d) #`(=/=* #,(data d))) (c-D c)))))))

  
;;
;; Partial relation application
;;

(define-syntax partial-apply
  (syntax-parser
    [(_ rep ((rel-staged rel-dyn) (x ...) ((~literal _) ...)))
     #'(partial-apply-rt rep 'rel-staged 'rel-dyn (list x ...))]))

(define (partial-apply-rt rep rel-staged rel-dyn args)
  (== rep (apply-rep rel-staged rel-dyn args #f)))

(define-syntax lpartial-apply
  (syntax-parser
    [(_ rep ((rel-staged rel-dyn) (x ...) ((~and y (~literal _)) ...)))     
     #:with (y-n ...) (generate-temporaries #'(y ...))
     #:with (y-n2 ...) (generate-temporaries #'(y ...))
     #'(fresh (y-n ...)
         (capture-later
          (fresh ()
            ;; This is a little subtle. This unification ends up as code in the
            ;; lambda body, but it has to be part of L in the capture to ensure
            ;; that substitution extensions to `y-n` are captured in the walk.
            (later #`(== #,(data y-n) y-n2))
            ...
            (rel-staged rep x ... y-n ...))
          (lambda (body)
            (l== rep (apply-rep
                      'rel-staged 'rel-dyn (list x ...)
                      #`(lambda (y-n2 ...)
                          (fresh ()
                            . #,body)))))))]))

(define-syntax apply-partial
  (syntax-parser
    [(_ rep ((rel-staged rel-dyn) ((~and x (~literal _)) ...) (y ...)))     
     #:with (x-n ...) (generate-temporaries #'(x ...))
     #'(fresh (proc x-n ...)
         ;; Note: the proc position doesn't actually unify, because a dynamic
         ;; and staged rep should be unifiable but one will have #f and the other
         ;; a procedure. So we have to walk the rep and check its field.
         (== rep (apply-rep 'rel-staged 'rel-dyn (list x-n ...) proc))
         (lambda (st)
           (let* ([rep (walk rep (state-S st))]
                  [proc (apply-rep-proc rep)])
             ((if (procedure? proc)
                  (proc y ...)
                  (rel-dyn x-n ... y ...))
              st))))]))

(define-syntax lapply-partial
  (syntax-parser
    [(_ rep ((rel-staged rel-dyn) ((~and x (~literal _)) ...) (y ...)))
     #'(later #`(apply-partial #,(data rep) ((rel-staged rel-dyn) (x ...) (#,(data y) ...))))]))


;;
;; condg
;;

(define-syntax condg
  (syntax-parser
   ((_ fallback ((x ...) (~and guard (g0 g ...)) (b0 b ...)) ...)
    #:with error-stx this-syntax
    #'(condg-runtime
       (lambda (st) (fallback st))
       (list
         (lambda (st)
           (let ([scope (subst-scope (state-S st))])
             (let ([x (var scope)] ...)
               (list
                 (bind* (g0 st) g ...)
                 (lambda (st) (bind* (b0 st) b ...))
                 #'guard))))
         ...)
       #'error-stx))))

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

(define (evaluate-guard thunk-stream guard-stx)
  (match (take 2 thunk-stream)
    ['() #f]
    [(list answer) answer]
    [answers 'nondet]))



;;
;; Reification
;;

;; (ListOf SyntaxWithData), Substitution -> (ListOf SyntaxWithDataVars)
;;
;; At reification time, walk* the lifted `later` syntax
;; and reflect the data within it to syntax for expressions that
;; construct the same data.
;;
;; The resulting syntax still contains data constructors containing
;; logic variables.
;;
;; called from mk.scm `reify`
(define (walk-later-final st)
  (for/list ([stx (reverse (state-L st))])
    (reflect-data-in-syntax (walk* stx (state-S st)))))

;; SyntaxWithData -> SyntaxWithDataVars
(define (reflect-data-in-syntax t)
  (map-syntax-with-data reflect-datum t))

;; Term -> SyntaxWithDataVars
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
      [(? syntax? t) (reflect-data-in-syntax t)]
      [(apply-rep name-staged name-dyn args proc)
       #`(apply-rep
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


;; SyntaxWithDataVars, ReifierSubst -> ReifierSubst
;;
;; Extends reify-S to traverse SyntaxWithDataVars. reify-S
;; is responsible for constructing a reifier substitution
;; that maps Vars to reified variable name symbols.
;;
;; called from mk.scm `reify-S`
(define (reify-S-syntax v S)
  (cond
    [(syntax? v)
     (reify-S-syntax (syntax-e v) S)]
    [(pair? v)
     (let ((S (reify-S-syntax (car v) S)))
       (reify-S-syntax (cdr v) S))]
    [(data? v)
     (reify-S (data-value v) S)]
    [else S]))


;;
;; Scope fixing
;;

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

by the way we construct lambdas in lpartial-apply, the parameter names do not intersect with logic variables, so we don't need to consider lambda especially in fix scope.

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


;;
;; Staging entry point
;;

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
  (map-syntax-with-data (lambda (v) v) x))

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

(define-syntax-rule (generate-staged (var ...) goal ...)
  (eval-syntax
   (gen-func
    (run 100 (var ... bogus-var1 bogus-var2) goal ...)
    'var ... 'bogus-var1 'bogus-var2)))

(define (invoke-staged f . args)
  (apply f (append args (list 'bogus-1 'bogus-2))))


(define (generated-code)
  (and res (syntax->datum res)))

(define (reset-generated-code!)
  (set! res #f))


;;
;; Staging entry point syntax
;;

(define-syntax run-staged
  (syntax-rules ()
    [(_ n (q0 q ...) g0 g ...)
     (let ()
       (printf "running first stage\n")
       (define f (generate-staged (q0 q ...) g0 g ...))
       (printf "running second stage\n")
       (run n (q0 q ...) (invoke-staged f q0 q ...)))]))

(define-syntax run-staged*
  (syntax-rules ()
    [(_ (q0 q ...) g0 g ...) (run-staged #f (q0 q ...) g0 g ...)]))

(define-syntax define-staged-relation
  (syntax-rules ()
    [(_ (name x0 x ...) g0 g ...)
     (define name
       (let ([f (time (generate-staged (x0 x ...) g0 g ...))])
         (lambda (x0 x ...)
           (invoke-staged f x0 x ...))))]))
