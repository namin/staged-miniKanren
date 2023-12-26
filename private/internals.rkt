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
(struct apply-rep [name args proc] #:prefab)

;; A SyntaxWithData is a syntax object containing (data TermWithIdentifiers) structures
;;  in some positions.
;;
;; A TermWithIdentifiers has term values but also syntax representing term variable references, like #'x
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
;; Staging-time control flow
;;

(define in-surrounding-fallback-evaluation? (make-parameter #f))

(define (succeed-in-fallback g)
  (lambda (st)
    (if (in-surrounding-fallback-evaluation?)
        st
        (g st))))

(define (fallback fallback-g g)
  (succeed-in-fallback
   (lambda (st)
     (let ([answers (parameterize ([in-surrounding-fallback-evaluation? #t])
                      (take 2 (lambda () (g st))))])
       (match answers
         ['() #f]
         [(list answer) (g st)]
         [answers (fallback-g st)])))))

(define (gather goal-thunk)
  (succeed-in-fallback
   (lambda (st-original)
     (let ((results (take #f (lambda () ((capture-later goal-thunk) st-original)))))
       (if (null? results)
           #f
           ((later #`(disj . #,results))
            st-original))))))

(define-syntax disj
  (syntax-rules ()
    [(_ g) g]
    [(_ g ...)
     (lambda (st)
       (suspend
        (let ((st (state-with-scope st (new-scope))))
          (mplus* (g st) ...))))]))

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
     #:with rel-annotated (syntax-property (datum->syntax #'fn (syntax-e #'rel))
                                           'fallback-function (syntax-e #'fn) #t)
     #'(later #`(invoke-fallback rel-annotated #,(data arg) ...))]))

(define-syntax lpartial-apply
  (syntax-rules ()
    [(_ rep (rel (x ...) (under ...)))
     (later #`(partial-apply #,(data rep) (rel (#,(data x) ...) (under ...))))]))

(define lsucceed (later #'succeed))
(define lfail (later #'fail))

;;
;; Scoped lift capturing
;;

;; (-> Goal) -> (-> State SyntaxWithData)
;; The goal argument is in a thunk to make sure that fresh variable allocations within
;; do not happen before we have captured the initial-var-idx. It's a bit of a nasty hack.
(define (capture-later goal-thunk)
  (lambda (st-original)
    (let* ([st-before (state-with-C st-original (C-new-later-scope (state-C st-original)))]
           [st-before (state-with-L st-before '())]
           [st-before (state-with-S st-before (new-subst-with-empty-exts (state-S st-before)))]
           [st-before (state-with-scope st-before (new-scope))])
      (define initial-var-idx (var-idx (var 'capture-later)))

      (bind
       ((goal-thunk) st-before)
       (lambda (st-after)
         (fresh-local-vars
          initial-var-idx
          (append (walk*-L (generate-constraints st-after) st-after)
                  (generate-subst-exts st-after initial-var-idx)
                  (walk*-L (reverse (state-L st-after)) st-after))))))))

;; Int, (ListOf SyntaxWithData) -> SyntaxWithData
(define (fresh-local-vars initial-var-idx L)
  (define local-vars (find-local-vars initial-var-idx L))
  (define local-var-ids (generate-temporaries local-vars))
  (define var-mapping (map cons local-vars local-var-ids))

  (define/syntax-parse (local-var-id ...) local-var-ids)
  (define/syntax-parse (L-closed ...) (map (lambda (stx) (replace-vars stx var-mapping)) L))

  #'(fresh (local-var-id ...)
      L-closed ...))

;; Int, (ListOf SyntaxWithData) -> (ListOf Var)
(define (find-local-vars initial-var-idx L)
  (define (rec v)
    (match v
      [(? var?)
       (if (var-local? v initial-var-idx)
           (set v)
           (set))]
      [(? syntax?) (rec (syntax-e v))]
      [(cons a d)
       (set-union (rec a) (rec d))]
      [(apply-rep name args proc)
       (set-union (rec args) (rec proc))]
      [(? data?)
       (rec (data-value v))]
      [else (set)]))

  (sort (set->list (rec L)) < #:key var-idx))

;; SyntaxWithData, (AList Var Identifier) -> SyntaxWithData
;; Example:
#; (#'(== #,(data x) #,(data (cons (var y) 1))), (list (cons (var y) #'y)))
#; ->
#; #'(== #,(data x) #,(data (cons #'y 1)))
(define (replace-vars stx var-mapping)
  (define (replace-in-datum v)
    (match v
      [(? var?)
       (let ([pr (assq v var-mapping)])
         (if pr
             (cdr pr)
             v))]
      [(cons a d)
       (cons (replace-in-datum a) (replace-in-datum d))]
      [(apply-rep name args proc)
       (apply-rep
        name
        (replace-in-datum args)
        (replace-in-datum proc))]
      [(? syntax?)
       (map-syntax-with-data (lambda (v) (data (replace-in-datum v))) v)]
      [else v]))
  
  (map-syntax-with-data (lambda (v) (data (replace-in-datum v))) stx))

(define (walk*-L L st)
  (for/list ([stx L])
    (walk* stx (state-S st))))

(define (new-subst-with-empty-exts S)
  (subst (subst-map S) (subst-scope S) '()))

(define (var-local? v initial-var-idx)
  (> (var-idx v) initial-var-idx))

(define (generate-subst-exts st initial-var-idx)
  (for/list ([b (reverse (subst-exts (state-S st)))]
             #:when (not (var-local? (car b) initial-var-idx)))
    #`(== #,(data (car b)) #,(data (walk* (cdr b) (state-S st))))))

(define (generate-constraints st)
  (let ([vars (remove-duplicates (reverse (C-vars (state-C st))))])
    (apply append (map (generate-var-constraints st) vars))))

;; TODO: this relies on internal details and only works for current set of type constraints.
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

(define (partial-apply-rt rep name args)
  (== rep (apply-rep name args #f)))

(define-syntax specialize-partial-apply
  (syntax-parser
    [(_ rep (rel (x ...) ((~and y (~literal _)) ...)))     
     #:with (y-var ...) (generate-temporaries #'(y ...))
     #:with (y-arg ...) (generate-temporaries #'(y ...))
     #'(specialize-partial-apply-rt
        (lambda ()
          (fresh (y-var ...)
            ;; This is a little subtle. This unification ends up as code in the
            ;; lambda body, but it has to be part of L in the capture to ensure
            ;; that substitution extensions to `y-n` are captured in the walk.
            (later #`(== #,(data y-var) y-arg))
            ...
            (rel rep x ... y-var ...)))
        rep 'rel (list x ...) (list #'y-arg ...))]))

(define (specialize-partial-apply-rt goal-thunk rep rel-name x-vals y-ids)
  (capture-later-and-then
   goal-thunk
   (lambda (result)
     (l== rep (apply-rep rel-name x-vals #`(lambda #,y-ids #,result))))))

;; (-> Goal), (-> SyntaxWithData Goal) -> Goal
(define (capture-later-and-then goal-thunk k)
  (lambda (st)
    (bind ((capture-later goal-thunk) st)
          (lambda (L) ((k L) st)))))

(define-syntax finish-apply
  (syntax-parser
    [(_ rep (rel ((~and x (~literal _)) ...) (y ...)))     
     #:with (x-n ...) (generate-temporaries #'(x ...))
     #'(fresh (x-n ...)
         (== rep (apply-rep 'rel (list x-n ...) 'doesnt-matter))
         (finish-apply-rt rep rel (list x-n ...) (list y ...)))]))
 
(define (finish-apply-rt rep rel-proc args1-vars args2-terms)
  (lambda (st)
    ;; The proc position of an apply-rep doesn't unify because a dynamic
    ;; rep and a staged rep should be unifiable but one will have #f and the other
    ;; a procedure. So we have to walk the rep manually to access its field.
    (define rep-proc (apply-rep-proc (walk rep (state-S st))))
    (define g (if (procedure? rep-proc)
                  (apply rep-proc args2-terms)
                  (apply rel-proc rep (append args1-vars args2-terms))))
    (g st)))

(define-syntax lfinish-apply
  (syntax-parser
    [(_ rep (rel ((~and x (~literal _)) ...) (y ...)))
     #'(later #`(finish-apply #,(data rep) (rel (x ...) (#,(data y) ...))))]))

;;
;; Reflecting data in lifted code to code that constructs the same data
;;

;; SyntaxWithData -> Syntax
(define (reflect-data-in-syntax t)
  (map-syntax-with-data reflect-term t))

;; TermWithIdentifiers -> Syntax
;;
;; Construct a syntax object representing an expression that will construct
;; the term value `t` when evaluated at runtime.
;;
;; Expects that all logic variables in the term have already been walked away
;; or replaced by identifiers referring to runtime fresh- or lambda- bindings,
;; and that constraints the variables have been turned into syntax elsewhere.
;;
;; Generates in order of preference: `quote` expressions where there are no
;; subexpressions that require evaluation; `list` constructor calls for proper
;; lists with elements that do require evaluation; and `cons` constructor calls.  
(define (reflect-term t)
  ;; TermWithIdentifiers -> (or Syntax Quotable)
  ;;
  ;; For each term, return either a value that can be constructed via `quote`,
  ;; or a syntax object that constructs the term. This allows us to construct
  ;; as large of quotations as possible.
  (define (quotable-or-reflected t)
    (match t
      [(or (? symbol?) (? number?) (? boolean?) (? string?)) t]
      [(? identifier?) t] ;; references inserted by replace-vars
      [(apply-rep name args proc-stx)
       #`(apply-rep '#,name
                    #,(reflect-term args)
                    #,(reflect-data-in-syntax proc-stx))]
      [(? list?)
       (define els-refl (map quotable-or-reflected t))
       (if (ormap syntax? els-refl)
           #`(list #,@(map to-expr els-refl))
           els-refl)]
      [(cons a d)
       (define a-refl (quotable-or-reflected a))
       (define d-refl (quotable-or-reflected d))
       (if (or (syntax? a-refl) (syntax? d-refl))
           #`(cons #,(to-expr a-refl) #,(to-expr d-refl))
           (cons a-refl d-refl))]))

  ;; (or Syntax Quotable) -> Syntax
  (define (to-expr v)
    (if (syntax? v) v #`(quote #,v)))

  (to-expr (quotable-or-reflected t)))

;;
;; Staging entry point
;;

(define res #f)

(define-syntax generate-staged
  (syntax-parser
    [(_ (v ...) goal ...)
     #:with (v-arg ...) (generate-temporaries #'(v ...))
     #'(generate-staged-rt
        (lambda ()
          (fresh (v ...)
            ;; see also specialize-partial-apply
            (later #`(== #,(data v) v-arg)) ...
            goal ...))
        (list #'v-arg ...))]))

(define (generate-staged-rt goal-thunk var-ids)
  (define stream (lambda () ((capture-later goal-thunk) empty-state)))
  (define (reflect-result result)
    #`(lambda #,var-ids #,(reflect-data-in-syntax result)))
  (define stx (check-unique-result (map reflect-result (take 2 stream))))
  (set! res stx)
  (eval-syntax stx))

(define (check-unique-result r)
  (match r
    [(list v) v]
    ['() (error 'staged "staging failed")]
    [else
     (for ([x r] [i (range (length r))])
       (printf "result ~a: ~a\n" (+ 1 i) x))
     (error 'staged "staging non-deterministic")]))

(define (generated-code)
  (and res (syntax->datum res)))

(define (reset-generated-code!)
  (set! res #f))