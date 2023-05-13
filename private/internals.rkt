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

;; condg is fallback-if-nondet + conde, I think?

;; StagingSearchResult is one of:
(struct ss:interleave [ss-k])
(struct ss:fail [])
(struct ss:notify-success [ss-k])
(struct ss:final-success [v ss-k])

;; ss-k : StagingSearchResultK
;; StagingSearchResultK = () -> StagingSearchResult

;; StagingSearchSuccessK = (State) -> StagingSearchResult
;; StagingGoal = (State, StagingSearchSuccessK) -> StagingSearchResult

(define (initial-k st) (ss:one-result st))

(define-syntax-rule
  (ss:suspend e)
  (ss:interleave (lambda () e)))

(define (ss:conj2 g1 g2)
  (lambda (st success-k)
    (g1 st (lambda (st) (g2 st success-k)))))

(define-syntax ss:conj
  (syntax-rules ()
    [(_ g1) g1]
    [(_ g1 g ...) (ss:conj2 g1 (ss:conj g ...))]))

(define-syntax-rule
  (ss:fresh (x ...) g0 g ...)
  (lambda (st success-k)
    (ss:suspend
     (let ((x (var (new-scope))) ...) ;; always use a new scope, so never use set-var-val!
       ((ss:conj g0 g ...) st success-k)))))

(define (ss:atomic runtime-g)
  (lambda (st success-k)
    (let ([res (runtime-g st)])
      (if res
          (success-k res)
          (ss:fail)))))


(define (ss:one-result v)
  (ss:final-success v (lambda () (ss:fail))))

(define (ss:maybe-fallback fallback-g g)
  (lambda (st success-k)
    (define (no-success-yet ss-k)
      (displayln 'no-success-yet)
      (match (ss-k)
        [(ss:fail)
         (ss:fail)]
        [(ss:interleave ss-k^)
         (ss:interleave (lambda () (no-success-yet ss-k^)))]
        [(ss:notify-success ss-k^)
         (displayln 'no-success-yet->one-success-notified)
         (ss:notify-success
          (lambda ()
            (one-success-notified ss-k^)))]
        [(ss:final-success _ _)
         (error 'ss:maybe-fallback
                "invariant violation; should get notify-success first")]))

    (define (one-success-notified ss-k)
      (match (ss-k)
        [(ss:fail)
         (ss:fail)]
        [(ss:interleave ss-k^)
         (ss:interleave (lambda () (one-success-notified ss-k^)))]
        [(ss:notify-success ss-k^)
         (displayln 'one-success-notified->fallback)
         (fallback-g st success-k)]
        [(ss:final-success v ss-k^)
         (have-result v ss-k^)]))

    (define (have-result v ss-k)
      (displayln 'have-result)
      (match (ss-k)
        [(ss:fail)
         (ss:one-result v)]
        [(ss:interleave ss-k^)
         (ss:interleave (lambda () (have-result v ss-k^)))]
        [(ss:notify-success ss-k^)
         (displayln 'have-result->fallback)
         (fallback-g st success-k)]
        [(ss:final-success v ss-k^)
         (error 'ss:maybe-fallback
                "invariant violation; should get notify-success first")]))

    (define (success-k^ st)
      (ss:notify-success (lambda () (success-k st))))
    
    (no-success-yet (lambda () (g st success-k^)))))

(define (ss:disj2 g1 g2)
  (lambda (st success-k)
    (define (node ss-k1 ss-k2)
      (match (ss-k1)
        [(ss:fail)
         (ss-k2)]
        [(ss:interleave ss-k1^)
         (ss:interleave (lambda () (node ss-k2 ss-k1^)))]
        [(ss:notify-success ss-k1^)
         (ss:notify-success (lambda () (node ss-k2 ss-k1^)))]
        [(ss:final-success v ss-k1^)
         (ss:final-success v (lambda () (node ss-k2 ss-k1^)))]))
    (ss:suspend (node (lambda () (g1 st success-k)) (lambda () (g2 st success-k))))))

(define-syntax ss:disj
  (syntax-rules ()
    [(_ g1) g1]
    ;; associativity doesn't matter either way, at staging time we get all answers
    [(_ g1 g ...) (ss:disj2 g1 (ss:disj g ...))]))

(define-syntax-rule
  (ss:conde (g ...) ...)
  (ss:disj (ss:conj g ...) ...))

(define (ss:take n ss-k)
  (if (and n (zero? n))
      '()
      (match (ss-k)
        [(ss:fail)
         '()]
        [(ss:interleave ss-k^)
         (ss:take n ss-k^)]
        [(ss:notify-success ss-k^)
         (ss:take n ss-k^)]
        [(ss:final-success v ss-k^)
         (cons v (ss:take (and n (- n 1)) ss-k^))])))

(define (ss:simple-run g)
  (ss:take 2 (lambda () (g empty-state initial-k))))

(define-syntax ss:generate-staged
  (syntax-parser
    [(_ (var ...) goal ...)
     #:with (var2 ...) (generate-temporaries (attribute var))
     #'(ss:generate-staged-rt
        (ss:fresh (var ...)
                  (ss:capture-later
                   (ss:fresh ()
                             (ss:atomic (later #`(== #,(data var) var2))) ...
                             goal ...)
                   (lambda (L)
                     (lambda (old-st success-k)
                       (success-k L)))))
        #'(var2 ...))]))

(define (ss:generate-staged-rt goal var-list)
  (define result
    (unique-result
     (ss:take 2
              (lambda ()
                (goal empty-state initial-k)))))
 
  (define reflected (reflect-data-in-syntax result))
  (define assigned (assign-vars reflected))

  (define stx
    (fix-scope-syntax
     #`(lambda #,var-list
         (fresh ()
           #,@assigned))))

  (set! res stx)

  (eval-syntax stx))

(define (ss:capture-later g k)
  (lambda (st-original success-k)
    (let* ([st-before (state-with-C st-original (C-new-later-scope (state-C st-original)))]
           [st-before (state-with-L st-before '())]
           [st-before (state-with-scope st-before (new-scope))])
      (g st-before
         (lambda (st-after)
           (let ([captured-L (for/list ([stx (append (reverse (state-L st-after))
                                                     (generate-constraints st-after))])
                               (walk* stx (state-S st-after)))])
             ((k captured-L)
              st-original
              success-k)))))))

(define-syntax ss:lconde
  (syntax-rules ()
    ((_ (g ...) ...)
     (ss:ldisj (ss:fresh () g ...) ...))))

(define (ss:ldisj . gs-init)
  (let recur ([gs gs-init] [gs-stx '()])
    (if (null? gs)
        (ss:atomic (later #`(conde #,@(reverse gs-stx))))
        (ss:capture-later (car gs)
                          (lambda (g-stx)
                            (recur (cdr gs) (cons g-stx gs-stx)))))))

(define-syntax ss:lpartial-apply
  (syntax-parser
    [(_ rep ((rel-staged rel-dyn) (x ...) ((~and y (~literal _)) ...)))     
     #:with (y-n ...) (generate-temporaries #'(y ...))
     #:with (y-n2 ...) (generate-temporaries #'(y ...))
     #'(ss:fresh (y-n ...)
         (ss:capture-later
          (ss:fresh ()
            ;; This is a little subtle. This unification ends up as code in the
            ;; lambda body, but it has to be part of L in the capture to ensure
            ;; that substitution extensions to `y-n` are captured in the walk.
            (ss:atomic (later #`(== #,(data y-n) y-n2)))
            ...
            (rel-staged rep x ... y-n ...))
          (lambda (body)
            (ss:atomic
             (l== rep (apply-rep
                      'rel-staged 'rel-dyn (list x ...)
                      #`(lambda (y-n2 ...)
                          (fresh ()
                            . #,body))))))))]))

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
           (map (lambda (atom) #`(absento #,(data atom) #,(data v))) (c-A c))
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

(define res #f)

(define-syntax generate-staged
  (syntax-parser
    [(_ (var ...) goal ...)
     #:with (var2 ...) (generate-temporaries (attribute var))
     #'(generate-staged-rt
        (fresh (var ...)
          (capture-later
           (fresh ()
             (later #`(== #,(data var) var2)) ...
             goal ...)
           (lambda (L)
             (lambda (old-st)
               L))))
        #'(var2 ...))]))

(define (assign-vars v)
  (let ((R (reify-S v (subst empty-subst-map nonlocal-scope))))
    (walk* v R)))

(require racket/pretty)

(define (generate-staged-rt goal var-list)
  (define result
    (unique-result
     (take 2
           (suspend
            (goal empty-state)))))
 
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
       (run n (q0 q ...) (f q0 q ...)))]))

(define-syntax run-staged*
  (syntax-rules ()
    [(_ (q0 q ...) g0 g ...) (run-staged #f (q0 q ...) g0 g ...)]))

(define-syntax define-staged-relation
  (syntax-rules ()
    [(_ (name x0 x ...) g0 g ...)
     (define name (time (generate-staged (x0 x ...) g0 g ...)))]))
