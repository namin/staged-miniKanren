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

(define (ss:fallback fallback-g g)
  (succeed-in-fallback
   (lambda (st)
     (let ([answers (parameterize ([in-surrounding-fallback-evaluation? #t])
                      (take 2 (lambda () (g st))))])
       (match answers
         ['() #f]
         [(list answer) (g st)]
         [answers (fallback-g st)])))))

(define (ss:gather g)
  (succeed-in-fallback
   (lambda (st-original)
     (let ((results (take #f (lambda () ((ss:capture-later g) st-original)))))
       (if (null? results)
           #f
           ((ss:later #`(disj . #,results))
            st-original))))))

(define-syntax conj
  (syntax-rules ()
    [(_ g) g]
    [(_ g0 g ...) (lambda (st) (bind* (g0 st) g ...))]))

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
     #:with rel-annotated (syntax-property (datum->syntax #'fn (syntax-e #'rel))
                                           'fallback-function (syntax-e #'fn) #t)
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

;; Goal, (-> SyntaxWithData Goal) -> Goal
(define (ss:capture-later-and-then g k)
  (lambda (st)
    (bind*
     st
     (ss:capture-later g)
     (lambda (L) ((k L) st)))))

;; Goal -> (-> State SyntaxWithData)
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
      [(apply-rep name-staged name-dyn args proc)
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
      [(apply-rep name-staged name-dyn args proc)
       (apply-rep
        name-staged
        name-dyn
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

(define generate-subst-exts
  (lambda (st initial-var-idx)
    (let* ((S (state-S st))
           (exts (subst-exts S)))
      (for/list ([b (reverse exts)]
                 #:when (not (var-local? (car b) initial-var-idx)))
        #`(== #,(data (car b)) #,(data (walk* (cdr b) (state-S st))))))))

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

(define (partial-apply-rt rep rel args)
  (== rep (apply-rep rel rel args #f))) ;; TODO: maybe have just one rel

(define-syntax ss:specialize-partial-apply
  (syntax-parser
    [(_ rep (rel (x ...) ((~and y (~literal _)) ...)))     
     #:with (y-n ...) (generate-temporaries #'(y ...))
     #:with (y-n2 ...) (generate-temporaries #'(y ...))
     #'(fresh ()
         (ss:capture-later-and-then
          (lambda (st) ;; nasty hack---make sure we don't allocate the variables until this goal is called!
            ((fresh (y-n ...)
               ;; This is a little subtle. This unification ends up as code in the
               ;; lambda body, but it has to be part of L in the capture to ensure
               ;; that substitution extensions to `y-n` are captured in the walk.
               (ss:later #`(== #,(data y-n) y-n2))
               ...
               (rel rep x ... y-n ...))
             st))
          (lambda (body)
            (l== rep (apply-rep
                      'rel 'rel (list x ...)
                      #`(lambda (y-n2 ...)
                          #,body))))))]))

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
           (let* ([repv (walk rep (state-S st))]
                  [proc (apply-rep-proc repv)])
             ((if (procedure? proc)
                  (proc y ...)
                  (rel repv x-n ... y ...))
              st))))]))

(define-syntax lfinish-apply
  (syntax-parser
    [(_ rep (rel ((~and x (~literal _)) ...) (y ...)))
     #'(ss:later #`(finish-apply #,(data rep) (rel (x ...) (#,(data y) ...))))]))

;;
;; Reflect
;;

;; (or/c (ListOf SyntaxWithData) SyntaxWithData) -> SyntaxWithDataVars
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
    [(_ (v ...) goal ...)
     #'(let ([v (var (new-scope))] ...)
         (ss:generate-staged-rt
          (ss:capture-later
           (fresh () goal ...))
          (list v ...)
          (list #'v ...)))]))

(define (ss:generate-staged-rt goal var-vals var-ids)
  (define result
    (unique-result
     (take 2 (lambda () (goal empty-state)))))

  (define var-mapping (map cons var-vals var-ids))
  (define closed (replace-vars result var-mapping))
  
  ;; TODO: We call reflect-data-in-syntax via walk-later-final in reify in mk.scm.
  ;; Why do we have to do it again here? Should it be removed from one or other?
  (define reflected (reflect-data-in-syntax closed))
  
  (define stx #`(lambda #,var-ids #,reflected))

  (set! res stx)

  (eval-syntax stx))

(define (generated-code)
  (and res (syntax->datum res)))

(define (reset-generated-code!)
  (set! res #f))
