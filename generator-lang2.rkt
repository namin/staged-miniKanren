#lang racket/base

(provide
 quote
 cons
 list
 #;racket-term
 define-term-syntax-rule
 ==
 apply-partial
 =/=
 absento
 stringo
 numbero
 symbolo
 fresh
 conde
 condg
 staged
 later
 now
 fail
 trace

 defrel
 defrel-partial
 defrel/generator
 run
 run*

 (rename-out [g:generated-code generated-code])
 
 quasiquote
 (for-space mk quasiquote)
 unquote)

;; https://github.com/michaelballantyne/syntax-spec
(require syntax-spec
         ;; for a nasty workaround
         (for-syntax (only-in ee-lib compile-reference lookup))
         (for-syntax racket/base
                     syntax/parse
                     racket/match
                     racket/list
                     syntax/id-set)
         
         (prefix-in g: "generator-lang.rkt")
         (only-in "staged-load.rkt" generate-staged invoke-staged project succeed))

(begin-for-syntax
  (struct runtime-rel [args-count] #:prefab)
  (struct generator-rel [args-count] #:prefab)
  (struct partial-rel [now-args-count later-args-count] #:prefab)

  (define-persistent-symbol-table relation-info)
  (define-persistent-symbol-table defrel-partial-generator))

(syntax-spec
  (binding-class term-var)
  (binding-class relation-name)

  (extension-class term-macro
                   #:binding-space mk)

  (nonterminal quoted
    #:description "quoted value"
    n:number
    b:boolean
    s:id
    ()
    (a:quoted . d:quoted))

  (nonterminal term
    #:description "miniKanren term"
    #:allow-extension term-macro

    ((~literal quote) t:quoted)
    ((~literal cons) t1:term t2:term)
    ((~literal list) t:term ...)

    (#%term-var x:term-var)
    ;; TODO: we don't check that the value of e is a valid term value.
    (racket-term e:racket-expr)
    
    (~> v:id
        (if (lookup #'v (binding-class-predicate term-var))
            #'(#%term-var v)
            #'(racket-term v)))

    (~> n:number
        #'(quote n))
    (~> b:boolean
        #'(quote b)))
  
  (nonterminal goal
    #:bind-literal-set goal-literals

    (== v:term-var ((~datum partial-apply) rel:relation-name arg:term ...))
    (== t1:term t2:term)
  
    (apply-partial v:term-var rel:relation-name arg:term ...)

    (=/= t1:term t2:term)
    (absento t1:term t2:term)
    (symbolo t1:term)
    (numbero t1:term)
    (stringo t1:term)

    (trace name:id t:term-var ...)

    (fresh (x:term-var ...) g:goal ...+)
    #:binding {(bind x) g}
    
    (conde [g:goal ...+] ...+)
    
    #;(~>/form (condg #:fallback f [(~optional (fresh x:id ...)) guard ...+ (~datum --) body ...] ...)
               #'(condg #:fallback f ((~? [x ...] []) [guard ...] [body ...]) ...))
    (condg #:fallback g:goal c:condg-clause ...+)
    
    
    (staged g:goal)
    (later g:goal)
    (now g:goal)
    
    fail

    (#%rel-app r:relation-name arg:term ...)
    (~> (r:id arg ...)
        #'(#%rel-app r arg ...)))

  (nonterminal condg-clause
    ([x:term-var ...] [guard:goal ...] [body:goal ...])
    #:binding {(bind x) guard body})
  
  (host-interface/definition
    (defrel (r:relation-name arg:term-var ...)
      g:goal ...+)
    #:binding [(export r) {(bind arg) g}]
    #:lhs
    [(symbol-table-set! relation-info #'r (runtime-rel (length (attribute arg))))
     #'r]
    #:rhs
    [#'(lambda (arg ...)
         (g:fresh ()
           (compile-runtime-goal g) ...))])

  (host-interface/definition
    (defrel/generator (r:relation-name arg:term-var ...)
      g:goal ...+)
    #:binding [(export r) {(bind arg) g}]
    #:lhs
    [(symbol-table-set! relation-info #'r (generator-rel (length (attribute arg))))
     #'r]
    #:rhs
    [#'(lambda (arg ...)
         (g:fresh ()
           (compile-now-goal g) ...))])

  (nonterminal maybe-generator
    id:relation-name
    (~datum #f))
  
  (host-interface/definition
    (defrel-partial
      (r:relation-name [now-arg:term-var ...+] [later-arg:term-var ...+])
      #:generator gen:maybe-generator
      g:goal ...+)
    #:binding [(export r) {(bind now-arg later-arg) g}]
    #:lhs
    [(symbol-table-set!
      relation-info #'r
      (partial-rel (length (attribute now-arg)) (length (attribute later-arg))))
     (symbol-table-set! defrel-partial-generator #'r (syntax-parse #'gen [#f #f] [g:id #'g]))
     #'r]
    #:rhs
    [#'(lambda (now-arg ... later-arg ...)
         (g:fresh ()
           (compile-runtime-goal g) ...))])

  (host-interface/expression
    (run n:racket-expr (q:term-var ...+) g:goal ...+)
    #:binding {(bind q) g}
    #'(g:run n (q ...) (compile-runtime-goal g) ...))

  (host-interface/expression
    (run* (q:term-var ...+) g:goal ...+)
    #:binding {(bind q) g}
    #'(g:run* (q ...) (compile-runtime-goal g) ...)))

(begin-for-syntax
  (require syntax/transformer)
  (define term-var-transformer
    (make-variable-like-transformer
     (lambda (id)
       #`(term-variable #,id)))))

(define-syntax compile-term
  (syntax-parser
    #:literals (quote cons list #%term-var racket-term)
    [(_ (#%term-var x:id))
     #'x]
    [(_ (racket-term e))
     ;; Disable access to term variables right now because of bug in combination
     ;; with syntax-local-lift-expression in staged
     #'(with-reference-compilers (#;[term-var term-var-transformer])
         (unwrap-term e #'e))]
    [(_ (quote t))
     #'(quote t)]
    [(_ (cons t1 t2))
     #'(cons (compile-term t1) (compile-term t2))]
    [(_ (list t ...))
     #'(list (compile-term t) ...)]))

(begin-for-syntax
  (define-syntax-class binary-constraint
    #:literal-sets (goal-literals)
    (pattern ==
      #:attr c #'g:==
      #:attr l #'g:l==)
    (pattern =/=
      #:attr c #'g:=/=
      #:attr l #'g:l=/=)
    (pattern absento
      #:attr c #'g:absento
      #:attr l #'g:labsento))

  (define-syntax-class unary-constraint
    #:literal-sets (goal-literals)
    (pattern symbolo
      #:attr c #'g:symbolo
      #:attr l #'g:lsymbolo)
    (pattern numbero
      #:attr c #'g:numbero
      #:attr l #'g:lnumbero)
    (pattern stringo
      #:attr c #'g:stringo
      #:attr l #'g:lstringo))

  (define (free-vars-of-binding-form binder-vars body-goals)
    (define body-vars
      (for/fold ([body-vars (immutable-free-id-set)])
                ([g body-goals])
        (free-id-set-union body-vars (free-vars g))))
    (free-id-set-subtract
     body-vars
     (immutable-free-id-set binder-vars)))
  
  (define (free-vars goal-stx)
    (syntax-parse goal-stx
      #:literals (#%term-var quote fresh condg)
      [(#%term-var x) (immutable-free-id-set (list #'x))]
      [(quote _) (immutable-free-id-set)]
      [(fresh (x ...) g ...)
       (free-vars-of-binding-form (attribute x) (attribute g))]
      [(condg #:fallback gl clauses ...)
       (apply free-id-set-union
              (for/list ([clause (attribute clauses)])
                (define/syntax-parse ([x:id ...] [guard ...] [body ...]) clause)
                (free-id-set-union
                 (free-vars #'gl)
                 (free-vars-of-binding-form (attribute x)
                                            (append (attribute guard) (attribute body))))))]
      [(a . d)
       (free-id-set-union (free-vars #'a) (free-vars #'d))]
      [_ (immutable-free-id-set)])))

(define-syntax compile-runtime-goal
  (syntax-parser
    #:literal-sets (goal-literals)

    [(_ (trace id x ...))
     #'(project (x ...)
         (begin
           (displayln (list 'id x ...))
           succeed))]
    
    [(_ (#%rel-app r:id arg ...))
     (match (symbol-table-ref relation-info #'r)
       [(runtime-rel arg-count)
        (when (not (= arg-count (length (attribute arg))))
          (raise-syntax-error #f "wrong number of arguments to relation" #'r))
        #'(r (compile-term arg) ...)]
       [_ (raise-syntax-error #f "runtime relation application expects relation defined by defrel" #'r)])]
    
    [(_ (== v:id ((~datum partial-apply) rel:id arg ...)))
     (match (symbol-table-ref relation-info #'rel)
       [(partial-rel now-args-count later-args-count)
        (when (not (= now-args-count (length (attribute arg))))
          (raise-syntax-error #f "wrong number of now-stage arguments to relation" #'r))
        (with-syntax ([rel-dyn #'rel]
                      [rel-staged (compile-reference (symbol-table-ref defrel-partial-generator #'rel))]
                      [(later-placeholders ...) (make-list later-args-count #'_)])
          #'(g:reify-call v ((rel-staged rel-dyn) ((compile-term arg) ...) (later-placeholders ...))))]
       [_ (raise-syntax-error #f "partial-apply expects relation defined by defrel-partial" #'r)])]
    
    [(_ (apply-partial v:id rel:id arg ...))
     (match (symbol-table-ref relation-info #'rel)
       [(partial-rel now-args-count later-args-count)
        (when (not (= later-args-count (length (attribute arg))))
          (raise-syntax-error #f "wrong number of later-stage arguments to relation" #'r))
        
       (with-syntax ([rel-dyn #'rel]
                     [rel-staged (compile-reference (symbol-table-ref defrel-partial-generator #'rel))]
                     [(now-placeholders ...) (make-list now-args-count #'_)])
          #'(g:apply-reified v ((rel-staged rel-dyn) (now-placeholders ...) ((compile-term arg) ...))))]
       [_ (raise-syntax-error #f "apply-partial expects relation defined by defrel-partial" #'r)])]
    
    [(_ (constraint:binary-constraint t1 t2))
     #'(constraint.c (compile-term t1) (compile-term t2))]
    [(_ (constraint:unary-constraint t))
     #'(constraint.c (compile-term t))]
    
    [(_ (fresh (x:id ...) g ...))
     #'(g:fresh (x ...) (compile-runtime-goal g) ...)]
    [(_ (conde [g ...] ...))
     #'(g:conde [(compile-runtime-goal g) ...] ...)]
    [(_ (~and stx (condg #:fallback gl ([x:id ...] [guard ...] [body ...]) ...)))
     ;; preserve the srcloc for condg runtime errors
     (syntax/loc #'stx (g:condg (compile-runtime-goal gl)
                                ([x ...] [(compile-runtime-goal guard) ...]
                                         [(compile-runtime-goal body) ...]) ...))]
    [(_ fail)
     #'g:fail]
    [(_ (staged g))
     #:with (var ...) (free-id-set->list (free-vars #'g))
     #:with staged-f (syntax-local-lift-expression #'(generate-staged (var ...) (compile-now-goal g)))
     #'(invoke-staged staged-f var ...)]

    [(_ (~and stx (~or (later . _) (now . _))))
     (raise-syntax-error #f "not allowed in runtime goal" #'stx)]
    [_ (raise-syntax-error #f "unexpected goal syntax" this-syntax)]))

(define-syntax compile-now-goal
  (syntax-parser
    #:literal-sets (goal-literals)
    [(_ (trace id x ...))
     #'(project (x ...)
         (begin
           (displayln (list 'id x ...))
           succeed))]
    
    [(_ (#%rel-app r:id arg ...))
     (match (symbol-table-ref relation-info #'r)
       [(generator-rel arg-count)
        (when (not (= arg-count (length (attribute arg))))
          (raise-syntax-error #f "wrong number of arguments to relation" #'r))
        #'(r (compile-term arg) ...)]
       [_ (raise-syntax-error #f "generator relation application expects relation defined by defrel/generator" #'r)])]

    [(_ (~and stx (== v:id ((~datum partial-apply) rel:id arg ...))))
     (raise-syntax-error #f "partial-apply not supported in generator code" #'stx)]

    [(_ (constraint:binary-constraint t1 t2))
     #'(constraint.c (compile-term t1) (compile-term t2))]
    [(_ (constraint:unary-constraint t))
     #'(constraint.c (compile-term t))]
    
    [(_ (fresh (x:id ...) g ...))
     #'(g:fresh (x ...) (compile-now-goal g) ...)]
    [(_ (conde [g ...] ...))
     #'(g:conde [(compile-now-goal g) ...] ...)]
    [(_ (~and stx (condg #:fallback gl ([x:id ...] [guard ...] [body ...]) ...)))
     ;; preserve the srcloc for condg runtime errors
     (syntax/loc #'stx (g:condg (compile-now-goal gl)
                                ([x ...] [(compile-now-goal guard) ...]
                                         [(compile-now-goal body) ...]) ...))]
    [(_ fail) #'fail]

    [(_ (later g))
     #'(compile-later-goal g)]

    [(_ (~and stx (~or (apply-partial . _)
                       (staged . _)
                       (now . _))))
     (raise-syntax-error #f "not supported in generator code" #'stx)]    
    [_ (raise-syntax-error #f "unexpected goal syntax" this-syntax)]))

(define-syntax compile-later-goal
  (syntax-parser
    #:literal-sets (goal-literals)
    [(_ (#%rel-app r:id arg ...))
     (match (symbol-table-ref relation-info #'r)
       [(runtime-rel arg-count)
        (when (not (= arg-count (length (attribute arg))))
          (raise-syntax-error #f "wrong number of arguments to relation" #'r))
        #'(g:lapp r (compile-term arg) ...)]
       [_ (raise-syntax-error #f "generated-code relation application expects relation defined by defrel" #'r)])]


    [(_ (== v:id ((~datum partial-apply) rel:id arg ...)))
     (match (symbol-table-ref relation-info #'rel)
       [(partial-rel now-args-count later-args-count)
        (when (not (= now-args-count (length (attribute arg))))
          (raise-syntax-error #f "wrong number of now-stage arguments to relation" #'r))
        (with-syntax ([rel-dyn #'rel]
                      [rel-staged (compile-reference (symbol-table-ref defrel-partial-generator #'rel))]
                      [(later-placeholders ...) (make-list later-args-count #'_)])
          #'(g:lreify-call v ((rel-staged rel-dyn) ((compile-term arg) ...) (later-placeholders ...))))]
       [_ (raise-syntax-error #f "partial-apply expects relation defined by defrel-partial" #'r)])]
    
    [(_ (apply-partial v:id rel:id arg ...))
     (match (symbol-table-ref relation-info #'rel)
       [(partial-rel now-args-count later-args-count)
        (when (not (= later-args-count (length (attribute arg))))
          (raise-syntax-error #f "wrong number of later-stage arguments to relation" #'r))
        
       (with-syntax ([rel-dyn #'rel]
                     [rel-staged (compile-reference (symbol-table-ref defrel-partial-generator #'rel))]
                     [(now-placeholders ...) (make-list now-args-count #'_)])
          #'(g:lapply-reified v ((rel-staged rel-dyn) (now-placeholders ...) ((compile-term arg) ...))))]
       [_ (raise-syntax-error #f "apply-partial expects relation defined by defrel-partial" #'r)])]
   
    [(_ (constraint:binary-constraint t1 t2))
     #'(constraint.l (compile-term t1) (compile-term t2))]
    [(_ (constraint:unary-constraint t ))
     #'(constraint.l (compile-term t))]
    
    [(_ (fresh (x:id ...) g ...))
     #'(g:fresh (x ...) (compile-later-goal g) ...)]
    [(_ (conde [g ...] ...))
     #'(g:lconde [(compile-later-goal g) ...] ...)]
    [(_ fail) #'g:lfail]
    
    [(_ (now g))
     #'(compile-now-goal g)]
    
    [(_ (~and stx (~or (condg . _)
                       (staged . _)
                       (later . _))))
     (raise-syntax-error #f "not supported in generated code" #'stx)]
    [_ (raise-syntax-error #f "unexpected goal syntax" this-syntax)]))


(define-syntax define-syntax/space
  (syntax-parser
    [(_ name space rhs)
     #`(define-syntax #,((make-interned-syntax-introducer (syntax-e #'space)) #'name) rhs)]))

(define-syntax/space quasiquote mk
  (term-macro
   (syntax-parser 
     [(~describe
       "`<datum>"
       (_ q))
      (let recur ([stx #'q] [level 0])
        (syntax-parse stx #:datum-literals (unquote quasiquote)
          [(unquote e)
           (if (= level 0)
               #'e
               #`(cons (quote unquote) #,(recur #'(e) (- level 1))))]
          [(unquote . rest)
           (raise-syntax-error 'unquote "bad unquote syntax" stx)]
          [(quasiquote e)
           #`(cons (quote quasiquote) #,(recur #'(e) (+ level 1)))]
          [(a . d)
           #`(cons #,(recur #'a level) #,(recur #'d level))]
          [(~or* v:identifier v:number v:boolean v:string) #'(quote v)]
          [() #'(quote ())]))])))

(require ee-lib/errors racket/match)
(struct term-variable [value])
(define (unwrap-term v blame-stx)
  (match v
    [atom
     #:when (or (symbol? atom)
                (string? atom)
                (number? atom)
                (null? atom)
                (boolean? atom))
     atom]
    [(term-variable val)
     val]
    [(cons a d)
     (cons (unwrap-term a blame-stx) (unwrap-term d blame-stx))]
    [_ (raise-argument-error/stx 'term "term-or-term-variable?" v blame-stx)]))
    
(define-syntax define-term-syntax-rule
  (syntax-parser
    [(_ (name . pat)
        template)
     #'(define-syntax name
         (term-macro
          (syntax-rules ()
            [(_ . pat)
             template])))]))
