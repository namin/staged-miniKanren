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

;; modified version of == that adds extensions to subst-exts
(define (==/staging-time u v)
  (lambda (st)
    (let-values (((S^ added) (unify u v (state-S st))))
      (if S^
          (let* ((new-exts (append added (subst-exts S^)))
                 (S^ (subst (subst-map S^) (subst-scope S^) new-exts)))
            (and-foldl update-constraints (state-with-S st S^) added))
          #f))))

;;
;; Staging-time search
;;

(define (first-answer-stream stream st)
  (case-inf stream
    (() #f)
    ((f) (lambda () (first-answer-stream (f) st)))
    ((c) st)
    ((c f) st)))

(define in-surrounding-fallback-evaluation? (make-parameter #f))

(define (ss:fallback fallback-g g)
  (lambda (st)
    (if (in-surrounding-fallback-evaluation?)
        (first-answer-stream (g st) st)
        (let ([answers (parameterize
                           ([in-surrounding-fallback-evaluation? #t])
                         (take 2 (lambda () (g st))))])
          (match answers
            ['() #f]
            [(list answer) (g st)]
            [answers (fallback-g st)])))))
  
(define (ss:gather g)
  (lambda (st-original)
    (if (in-surrounding-fallback-evaluation?)
        (first-answer-stream (g st-original) st-original)
        (let ((results (take #f (lambda () ((ss:capture-later g) st-original)))))
          (if (null? results)
              #f
              ((ss:later
                (if (= (length results) 1)
                    (let ([result (car results)])
                      (if (= (length result) 1)
                          (car result)
                          ;; TODO: would a fresh () be okay here or would it break scope fixing?
                          #`(conj #,@result)))
                    #`(conde
                        #,@(for/list ([result results])
                             #`[#,@result]))))
               st-original))))))

(define-syntax conj
  (syntax-rules ()
    [(_ g) g]
    [(_ g0 g ...) (lambda (st) (bind* (g0 st) g ...))]))


;;
;; Basic "later" constraint and goal variants
;;

(define (ss:later x)
  (lambda (st)
    (state-with-L st (cons x (state-L st)))))

(define (later-binary-constraint constraint-id)
  (lambda (t1 t2)
    (ss:later #`(#,constraint-id #,(data t1) #,(data t2)))))

(define (later-unary-constraint constraint-id)
  (lambda (t)
    (ss:later #`(#,constraint-id #,(data t)))))

(define-values (l== l=/= labsento)
  (apply values (map later-binary-constraint (list #'== #'=/= #'absento))))

(define-values (lsymbolo lnumbero lstringo)
  (apply values (map later-unary-constraint (list #'symbolo #'numbero #'stringo))))

(define-syntax lapp
  (syntax-rules ()
    [(_ relation arg ...)
     (ss:later #`(relation #,(data arg) ...))]))

(define-syntax invoke-fallback
  (syntax-parser
    [(_ rel arg ...)
     #:with fn (datum->syntax #'rel (syntax-property #'rel 'fallback-function))
     #'(fn arg ...)]))

(define-syntax linvoke-fallback
  (syntax-parser
    [(_ rel fn arg ...)
     ;; We want rel-annotated to be a nice human readable name, whereas `fn` is a lifted name we can't control.
     ;; When we generate here for eval, use the human readable symbol. We'll put the lifted symbol in the syntax
     ;; property and the lifted's lexical context on the rel-annotated, and reunite them in invoke-fallback.
     ;; We need this trick because of the expander's shortcomings re: adjusting references in syntax properties.
     #:with rel-annotated (syntax-property (datum->syntax #'fn (syntax-e #'rel)) 'fallback-function (syntax-e #'fn) #t)
     #'(ss:later #`(invoke-fallback rel-annotated #,(data arg) ...))]))

(define-syntax lpartial-apply
  (syntax-rules ()
    [(_ rep (rel (x ...) (under ...)))
     (ss:later #`(partial-apply #,(data rep) (rel (#,(data x) ...) (under ...))))]))

(define lsucceed (ss:later #'succeed))
(define lfail (ss:later #'fail))


;;
;; Scoped lift capturing
;;

(define (ss:capture-later-and-then g k)
  (lambda (st)
    (bind*
     st
     (ss:capture-later g)
     (lambda (L) ((k L) st)))))

(define (ss:capture-later g)
  (lambda (st-original)
    (let* ([st-before (state-with-C st-original (C-new-later-scope (state-C st-original)))]
           [st-before (state-with-L st-before '())]
           [st-before (state-with-S st-before (new-subst-with-empty-exts (state-S st-before)))]
           [st-before (state-with-scope st-before (new-scope))])
      (define initial-var-idx (var-idx (var 'capture-later)))

      (bind
       (g st-before)
       (lambda (st-after)
         (append (walk*-L (generate-constraints st-after) st-after)
                 (generate-subst-exts st-after initial-var-idx)
                 (walk*-L (reverse (state-L st-after)) st-after)))))))


(define (walk*-L L st)
  (for/list ([stx L])
    (walk* stx (state-S st))))

(define (new-subst-with-empty-exts S)
  (subst (subst-map S) (subst-scope S) '()))

(define generate-subst-exts
  (lambda (st initial-var-idx)
    (let* ((S (state-S st))
           (exts (subst-exts S)))
      (for/list ([b (reverse exts)]
                 #:when (<= (var-idx (car b)) initial-var-idx))
        #`(== #,(data (car b)) #,(data (walk* (cdr b) (state-S st))))))))

(define (generate-constraints st)
  (let ([vars (remove-duplicates (reverse (C-vars (state-C st))))])
    (apply append (map (generate-var-constraints st) vars))))

;; TODO: this relies on internal details, only works for current set of type constraints.
;;  Should figure how to make generic in type constraints at least.
(define (generate-var-constraints st)
  (lambda (v)
    (let ([c (lookup-c st v)])
      (if (eq? c empty-c)
          '()
          (append
           (if (c-T c)
               (let ((cid (hash-ref (hasheq 'sym #'symbolo 'num #'numbero 'str #'stringo)
                                    (type-constraint-reified (c-T c)))))
                 (list #`(#,cid #,(data v))))
               '())
           (map (lambda (atom) #`(absento #,(data atom) #,(data v))) (c-A c))
           (map (lambda (d) #`(=/=* #,(data d))) (c-D c)))))))

;;
;; Partial relation application
;;

(define-syntax partial-apply
  (syntax-parser
    [(_ rep (rel (x ...) ((~literal _) ...)))
     #'(partial-apply-rt rep 'rel (list x ...))]))

(define (partial-apply-rt rep rel args)
  (== rep (apply-rep rel rel args #f))) ;; TODO: maybe have just one rel

(define-syntax ss:specialize-partial-apply
  (syntax-parser
    [(_ rep (rel (x ...) ((~and y (~literal _)) ...)))     
     #:with (y-n ...) (generate-temporaries #'(y ...))
     #:with (y-n2 ...) (generate-temporaries #'(y ...))
     #'(fresh (y-n ...)
         (ss:capture-later-and-then
          (fresh ()
            ;; This is a little subtle. This unification ends up as code in the
            ;; lambda body, but it has to be part of L in the capture to ensure
            ;; that substitution extensions to `y-n` are captured in the walk.
            (ss:later #`(== #,(data y-n) y-n2))
            ...
            (rel rep x ... y-n ...))
          (lambda (body)
            (l== rep (apply-rep
                      'rel 'rel (list x ...)
                      #`(lambda (y-n2 ...)
                          (fresh ()
                            . #,body)))))))]))

(define-syntax finish-apply
  (syntax-parser
    [(_ rep (rel ((~and x (~literal _)) ...) (y ...)))     
     #:with (x-n ...) (generate-temporaries #'(x ...))
     #'(fresh (proc x-n ...)
         ;; Note: the proc position doesn't actually unify, because a dynamic
         ;; and staged rep should be unifiable but one will have #f and the other
         ;; a procedure. So we have to walk the rep and check its field.
         (== rep (apply-rep 'rel 'rel (list x-n ...) proc))
         (lambda (st)
           (let* ([rep (walk rep (state-S st))]
                  [proc (apply-rep-proc rep)])
             ((if (procedure? proc)
                  (proc y ...)
                  (rel rep x-n ... y ...))
              st))))]))

(define-syntax lfinish-apply
  (syntax-parser
    [(_ rep (rel ((~and x (~literal _)) ...) (y ...)))
     #'(ss:later #`(finish-apply #,(data rep) (rel (x ...) (#,(data y) ...))))]))

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

  (struct literal [v] #:prefab)
  (struct expr [v] #:prefab)

  (define (to-expr-v v)
    (match v
      [(expr v) v]
      [(literal v) #`(quote #,v)]))
  
  (define (reflect t)
    (match t
      [(? nonliteral-datum?) (expr (reflect-nonliteral-datum t))]
      [(? list?)
       (define els-refl (map reflect t))
       (if (andmap literal? els-refl)
           (literal (map literal-v els-refl))
           (expr #`(list #,@(map to-expr-v els-refl))))]
      [(cons a d)
       (let ([a-refl (reflect a)] [d-refl (reflect d)])
         (match* (a-refl d-refl)
           [((literal a-lit) (literal d-lit))
            (literal (cons a-lit d-lit))]
           [(a d)
            (expr #`(cons #,(to-expr-v a) #,(to-expr-v d)))]))]
      [else (literal t)]))

  (to-expr-v (reflect t)))


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

(define res #f)

(define-syntax ss:generate-staged
  (syntax-parser
    [(_ (var ...) goal ...)
     #:with (var2 ...) (generate-temporaries (attribute var))
     #'(ss:generate-staged-rt
        (fresh (var ...)
          (ss:capture-later
           (fresh ()
             (ss:later #`(== #,(data var) var2)) ...
             goal ...)))
        #'(var2 ...))]))


(define (assign-vars v)
  (let ((R (reify-S v (subst empty-subst-map nonlocal-scope '()))))
    (walk* v R)))

(define (ss:generate-staged-rt goal var-list)
  (define result
    (unique-result
     (take 2 (lambda () (goal empty-state)))))
 
  (define reflected (reflect-data-in-syntax result))
  (define assigned (assign-vars reflected))

  (define stx
    (fix-scope-syntax
     #`(lambda #,var-list
         (fresh ()
           #,@assigned))))

  (set! res stx)

  (eval-syntax stx))

(define (generated-code)
  (and res (syntax->datum res)))

(define (reset-generated-code!)
  (set! res #f))
