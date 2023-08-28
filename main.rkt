#lang racket/base

(provide
 quote
 cons
 list
 #;racket-term
 define-term-syntax-rule
 ==
 =/=
 absento
 stringo
 numbero
 symbolo
 fresh
 conde
 partial-apply
 specialize-partial-apply
 finish-apply
 fallback
 gather
 staged
 time-staged
 later
 fail
 trace

 defrel
 defrel/staged
 defrel-partial
 defrel-partial/staged
 defrel/staged/fallback
 run
 run*

 (rename-out [i:generated-code generated-code])
 
 quasiquote
 (for-space mk quasiquote)
 unquote)

;; https://github.com/michaelballantyne/syntax-spec
(require syntax-spec
         ;; for a nasty workaround
         (for-syntax (only-in ee-lib compile-reference lookup compile-binder! compiled-from))
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/match
                     racket/list
                     syntax/id-set)
         
         (prefix-in i:  "private/internals.rkt"))

(begin-for-syntax
  ;; stage is one of: runtime, staging-time, multistage
  (struct simple-rel [stage args-count] #:prefab)
  (struct partial-rel [stage args1-count args2-count] #:prefab)

  (define-persistent-symbol-table relation-info)
  (define-persistent-symbol-table staged-relation-generator)

  (define (register-simple-rel! name stage args)
    (symbol-table-set! relation-info name (simple-rel stage (length args))))

  (define (check-simple-rel name use-stage use-args)
    (match (symbol-table-ref relation-info name)
      [(simple-rel def-stage def-arg-count)
       (unless (or (eq? def-stage 'multistage) (eq? def-stage use-stage))
         (raise-syntax-error #f (format "def stage ~a should match use stage ~a for relation" def-stage use-stage) name))
       (when (not (= def-arg-count (length use-args)))
         (raise-syntax-error #f "wrong number of arguments to relation" name))
       def-stage]
      [_ (raise-syntax-error #f "relation application expects relation" name)]))

  (define (name-generator! name)
    (compile-binder! (compiled-from name) #:table staged-relation-generator))

  (define (reference-generator name)
    (compile-reference (compiled-from name) #:table staged-relation-generator))
  )

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
    
    (== t1:term t2:term)
    (=/= t1:term t2:term)
    (absento t1:term t2:term)
    (symbolo t1:term)
    (numbero t1:term)
    (stringo t1:term)

    (trace name:id t:term-var ...+)

    (fresh (x:term-var ...) g:goal ...+)
    #:binding {(bind x) g}
    
    (conde [g:goal ...+] ...+)

    (partial-apply v:term-var rel:relation-name arg:term ...)
    (specialize-partial-apply v:term-var rel:relation-name arg:term ...)
    (finish-apply v:term-var rel:relation-name arg:term ...)

    (fallback body:goal)
    (fallback fb:goal body:goal)
    (gather body:goal)
    
    (staged g:goal)
    (time-staged g:goal)
    (later g:goal)
    
    fail

    (#%rel-app r:relation-name arg:term ...)
    (#%rel-app/fallback r:id arg:term ...)
    (~> (r:id arg ...)
        #'(#%rel-app r arg ...)))
  
  (host-interface/definition
   (defrel (r:relation-name arg:term-var ...)
     g:goal ...+)
   #:binding [(export r) {(bind arg) g}]
   #:lhs
   [(register-simple-rel! #'r 'runtime (attribute arg))
    #'r]
   #:rhs
   [#'(lambda (arg ...)
        (i:relation-body
         (compile-runtime-goal g) ...))])

  (host-interface/definition
   (defrel/staged (r:relation-name arg:term-var ...)
     g:goal ...+)
   #:binding [(export r) {(bind arg) g}]
   #:lhs
   [#:with r-generator (name-generator! #'r)
    (register-simple-rel! #'r 'multistage (attribute arg))
    #'(r-generator r)]
   #:rhs
   [#:with (gl ...) (lift-fallbacks! (attribute g) #'r)
    #'(values
       (lambda (arg ...)
         (i:relation-body
          (compile-now-goal gl) ...))
       (lambda (arg ...)
         (i:relation-body
          (compile-now-for-runtime-goal gl) ...)))])

  (host-interface/definition
   (defrel-partial
     (r:relation-name rep:term-var [now-arg:term-var ...+] [later-arg:term-var ...+])
     g:goal ...)
   #:binding [(export r) {(bind rep now-arg later-arg) g}]
   #:lhs
   [(symbol-table-set!
     relation-info #'r
     (partial-rel 'runtime (length (attribute now-arg)) (length (attribute later-arg))))
    #'r]
   #:rhs
   [#'(lambda (rep now-arg ... later-arg ...)
        (i:relation-body
         (compile-runtime-goal g) ...))])

  (host-interface/definition
   (defrel-partial/staged
     (r:relation-name rep:term-var [now-arg:term-var ...+] [later-arg:term-var ...+])
     g:goal ...)
   #:binding [(export r) {(bind rep now-arg later-arg) g}]
   #:lhs
   [#:with r-generator (name-generator! #'r)
    (symbol-table-set!
     relation-info #'r
     (partial-rel 'multistage (length (attribute now-arg)) (length (attribute later-arg))))
    #'(r-generator r)]
   #:rhs
   [#:with (gl ...) (lift-fallbacks! (attribute g) #'r)
    #'(values
       (lambda (rep now-arg ... later-arg ...)
         (i:relation-body
          (compile-now-goal gl) ...))
       (lambda (rep now-arg ... later-arg ...)
         (i:relation-body
          (compile-now-for-runtime-goal gl) ...)))])
  
  (host-interface/expression
   (run n:racket-expr (q:term-var ...+) g:goal ...+)
   #:binding {(bind q) g}
   #'(i:run n (q ...) (compile-runtime-goal g) ...))

  (host-interface/expression
   (run* (q:term-var ...+) g:goal ...+)
   #:binding {(bind q) g}
   #'(i:run* (q ...) (compile-runtime-goal g) ...)))

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
      #:attr c #'i:==
      #:attr l #'i:l==)
    (pattern =/=
      #:attr c #'i:=/=
      #:attr l #'i:l=/=)
    (pattern absento
      #:attr c #'i:absento
      #:attr l #'i:labsento))

  (define-syntax-class unary-constraint
    #:literal-sets (goal-literals)
    (pattern symbolo
      #:attr c #'i:symbolo
      #:attr l #'i:lsymbolo)
    (pattern numbero
      #:attr c #'i:numbero
      #:attr l #'i:lnumbero)
    (pattern stringo
      #:attr c #'i:stringo
      #:attr l #'i:lstringo))

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
      #:literals (#%term-var quote fresh)
      [(#%term-var x) (immutable-free-id-set (list #'x))]
      [(quote _) (immutable-free-id-set)]
      [(fresh (x ...) g ...)
       (free-vars-of-binding-form (attribute x) (attribute g))]
      [(a . d)
       (free-id-set-union (free-vars #'a) (free-vars #'d))]
      [_ (immutable-free-id-set)]))

  (define (lift-fallbacks! now-goals rel-name)
    (define counter!
      (let ([val 0])
        (lambda ()
          (set! val (+ 1 val))
          val)))
    
    (define (recur now-goal)
      (syntax-parse now-goal
        #:literal-sets (goal-literals)
        [(fallback body)
         #:with (var ...) (free-id-set->list (free-vars (attribute body)))
         #:with fallback-name (format-id #f "~a/~a" rel-name (counter!))
         #:with bodyl (recur (attribute body))
         #:with lifted (syntax-local-lift-expression
                        #'(lambda (var ...) (compile-now-for-runtime-goal bodyl)))
         #'(fallback (later (#%rel-app/fallback fallback-name lifted (#%term-var var) ...)) bodyl)]

        [(fallback fb body)
         #:with bodyl (recur (attribute body))
         #'(fallback fb bodyl)]
        [(fresh (x:id ...) g ...)
         #:with (gl ...) (map recur (attribute g))
         #'(fresh (x ...) gl ...)]
        [(conde [g ...] ...)
         #:with ([gl ...] ...) (map (lambda (l) (map recur l)) (attribute g))
         #'(conde [gl ...] ...)]
        [(gather body)
         #:with bodyl (recur (attribute body))
         #'(gather bodyl)]
        [_ this-syntax]))

    (map recur now-goals)))

(define-syntax compile-runtime-goal
  (syntax-parser
    #:literal-sets (goal-literals)

    [(_ (trace id x ...))
     #'(i:project (x ...)
                  (begin
                    (displayln (list 'id x ...))
                    i:succeed))]
    
    [(_ (#%rel-app r:id arg ...))
     (check-simple-rel #'r 'runtime (attribute arg))
     #'(r (compile-term arg) ...)]
    [(_ (#%rel-app/fallback r:id fn:id arg ...))
     #'(fn (compile-term arg) ...)]
    
    [(_ (partial-apply v:id rel:id arg ...))
     (match (symbol-table-ref relation-info #'rel)
       [(partial-rel stage now-args-count later-args-count)
        (when (not (= now-args-count (length (attribute arg))))
          (raise-syntax-error #f "wrong number of now-stage arguments to relation" #'r))
        (with-syntax ([(later-placeholders ...) (make-list later-args-count #'_)])
          #'(i:partial-apply v (rel ((compile-term arg) ...) (later-placeholders ...))))]
       [_ (raise-syntax-error #f "partial-apply expects relation defined by defrel-partial" #'r)])]

    [(_ (~and stx (specialize-partial-apply v:id rel:id arg ...)))
     (raise-syntax-error #f "not allowed in runtime goal" #'stx)]
    
    [(_ (finish-apply v:id rel:id arg ...))
     (match (symbol-table-ref relation-info #'rel)
       [(partial-rel stage now-args-count later-args-count)
        (when (not (= later-args-count (length (attribute arg))))
          (raise-syntax-error #f "wrong number of later-stage arguments to relation" #'r))
        
        (with-syntax ([(now-placeholders ...) (make-list now-args-count #'_)])
          #'(i:finish-apply v (rel (now-placeholders ...) ((compile-term arg) ...))))]
       [_ (raise-syntax-error #f "finish-apply expects relation defined by defrel-partial" #'r)])]
    
    [(_ (constraint:binary-constraint t1 t2))
     #'(constraint.c (compile-term t1) (compile-term t2))]
    [(_ (constraint:unary-constraint t))
     #'(constraint.c (compile-term t))]
    
    [(_ (fresh (x:id ...) g ...))
     #'(i:fresh (x ...) (compile-runtime-goal g) ...)]
    [(_ (conde [g ...] ...))
     #'(i:conde [(compile-runtime-goal g) ...] ...)]
    [(_ fail)
     #'i:fail]
    [(_ (staged g))
     #:with (var ...) (free-id-set->list (free-vars #'g))
     #:with (gl) (lift-fallbacks! (list #'g) #'staged)
     #:with staged-f (syntax-local-lift-expression #'(i:ss:generate-staged (var ...) (compile-now-goal gl)))
     #'(staged-f var ...)]
    [(_ (time-staged g))
     #:with (var ...) (free-id-set->list (free-vars #'g))
     #:with (gl) (lift-fallbacks! (list #'g) #'staged)
     #:with staged-f (syntax-local-lift-expression #'(time (i:ss:generate-staged (var ...) (compile-now-goal gl))))
     #'(staged-f var ...)]
    [(_ (~and stx (~or (later . _) (fallback . _) (gather . _))))
     (raise-syntax-error #f "not allowed in runtime goal" #'stx)]
    [_ (raise-syntax-error #f "unexpected goal syntax" this-syntax)]))

(define-syntax compile-now-goal
  (syntax-parser
    #:literal-sets (goal-literals)
    [(_ (trace id x ...))
     ;(error 'compile-now-goal "TODO not supported")
     #'(i:ss:project (x ...)
                     (begin
                       (displayln (list 'id x ...))
                       (i:ss:atomic i:succeed)))]
    
    [(_ (#%rel-app r:id arg ...))
     (check-simple-rel #'r 'staging-time (attribute arg))
     (define/syntax-parse r-generator (reference-generator #'r))
     #'(r-generator (compile-term arg) ...)]

    [(_ (~and stx (partial-apply v:id rel:id arg ...)))
     (raise-syntax-error #f "partial-apply not supported in staging-time code" #'stx)]

    [(_ (specialize-partial-apply v:id rel:id arg ...))
     (match (symbol-table-ref relation-info #'rel)
       [(partial-rel stage now-args-count later-args-count)
        (when (not (= now-args-count (length (attribute arg))))
          (raise-syntax-error #f "wrong number of now-stage arguments to relation" #'r))
        (with-syntax ([(later-placeholders ...) (make-list later-args-count #'_)]
                      [rel (reference-generator #'rel)])
          #'(i:ss:specialize-partial-apply v (rel ((compile-term arg) ...) (later-placeholders ...))))]
       [_ (raise-syntax-error #f "specialize-partial-apply expects relation defined by defrel-partial" #'r)])]

    [(_ (== t1 t2))
     #'(i:ss:atomic (i:==/staging-time (compile-term t1) (compile-term t2)))]
    [(_ (constraint:binary-constraint t1 t2))
     #'(i:ss:atomic (constraint.c (compile-term t1) (compile-term t2)))]
    [(_ (constraint:unary-constraint t))
     #'(i:ss:atomic (constraint.c (compile-term t)))]
    
    [(_ (fresh (x:id ...) g ...))
     #'(i:ss:fresh (x ...) (compile-now-goal g) ...)]
    [(_ (conde [g ...] ...))
     #'(i:ss:conde [(compile-now-goal g) ...] ...)]
    
    [(_ (fallback fb body))
     #'(i:ss:fallback
        (compile-now-goal fb)
        (compile-now-goal body))]

    [(_ (gather body))
     #'(i:ss:gather (compile-now-goal body))]
    
    [(_ fail) #'(i:ss:atomic i:fail)]

    [(_ (later g))
     #'(compile-later-goal g)]

    [(_ (~and stx (~or (finish-apply . _)
                       (staged . _))))
     (raise-syntax-error #f "not supported in staging-time code" #'stx)]    
    [_ (raise-syntax-error #f "unexpected goal syntax" this-syntax)]))

(define-syntax compile-now-for-runtime-goal
  (lambda (stx)
    (define/syntax-parse (_ now-goal) stx)
    (syntax-parse stx
      #:literal-sets (goal-literals)
      [(_ (trace id x ...))
       #'(compile-runtime-goal now-goal)]
      [(_ (#%rel-app r:id arg ...))
       #'(compile-runtime-goal now-goal)]
      [(_ (#%rel-app/fallback r:id fn:id arg ...))
       #'(compile-runtime-goal now-goal)]
      
      [(_ (~and stx (partial-apply v:id rel:id arg ...)))
       (raise-syntax-error #f "partial-apply not supported in staging-time code" #'stx)]
      [(_ (specialize-partial-apply v:id rel:id arg ...))
       #'(compile-runtime-goal (partial-apply v rel arg ...))]

      
      [(_ (== t1 t2))
       #'(compile-runtime-goal now-goal)]
      [(_ (constraint:binary-constraint t1 t2))
       #'(compile-runtime-goal now-goal)]
      [(_ (constraint:unary-constraint t))
       #'(compile-runtime-goal now-goal)]
     
      [(_ (fresh (x:id ...) g ...))
       #'(i:fresh (x ...) (compile-now-for-runtime-goal g) ...)]
      [(_ (conde [g ...] ...))
       #'(i:conde [(compile-now-for-runtime-goal g) ...] ...)]
     
      [(_ (~and stx (fallback fb body)))
       #;(raise-syntax-error #f "fallback not supported in multistage code" #'stx)
       #'(compile-now-for-runtime-goal fb)]

      [(_ (gather body))
       #'(compile-now-for-runtime-goal body)]
     
      [(_ fail)
       #'(compile-runtime-goal now-goal)]

      [(_ (later g))
       #'(compile-runtime-goal g)]

      [(_ (~and stx (~or (finish-apply . _)
                         (staged . _))))
       (raise-syntax-error #f "not supported in generator code" #'stx)]    
      [_ (raise-syntax-error #f "unexpected goal syntax" this-syntax)])))

(define-syntax compile-later-goal
  (syntax-parser
    #:literal-sets (goal-literals)
    [(_ (#%rel-app r:id arg ...))
     (check-simple-rel #'r 'runtime (attribute arg))
     #'(i:lapp r (compile-term arg) ...)]
    [(_ (#%rel-app/fallback r:id fn:id arg ...))
     #'(i:linvoke-fallback r fn (compile-term arg) ...)]

    [(_ (partial-apply v:id rel:id arg ...))
     (match (symbol-table-ref relation-info #'rel)
       [(partial-rel stage now-args-count later-args-count)
        (when (not (= now-args-count (length (attribute arg))))
          (raise-syntax-error #f "wrong number of now-stage arguments to relation" #'r))
        (with-syntax ([(later-placeholders ...) (make-list later-args-count #'_)])
          #'(i:lpartial-apply v (rel ((compile-term arg) ...) (later-placeholders ...))))]
       [_ (raise-syntax-error #f "partial-apply expects relation defined by defrel-partial" #'r)])]

    [(_ (~and stx (specialize-partial-apply v:id rel:id arg ...)))
     (raise-syntax-error #f "not supported in generated code" #'stx)]
    
    [(_ (finish-apply v:id rel:id arg ...))
     (match (symbol-table-ref relation-info #'rel)
       [(partial-rel stage now-args-count later-args-count)
        (when (not (= later-args-count (length (attribute arg))))
          (raise-syntax-error #f "wrong number of later-stage arguments to relation" #'r))
        
        (with-syntax ([(now-placeholders ...) (make-list now-args-count #'_)])
          #'(i:lfinish-apply v (rel (now-placeholders ...) ((compile-term arg) ...))))]
       [_ (raise-syntax-error #f "finish-apply expects relation defined by defrel-partial" #'r)])]
   
    [(_ (constraint:binary-constraint t1 t2))
     #'(constraint.l (compile-term t1) (compile-term t2))]
    [(_ (constraint:unary-constraint t))
     #'(constraint.l (compile-term t))]
    
    [(_ (fresh (x:id ...) g ...))
     #'(i:ss:fresh (x ...) (compile-later-goal g) ...)]
    [(_ (conde [g ...] ...))
     #'(i:ss:gather
        (i:ss:conde
         [(compile-later-goal g) ...] ...))]
    [(_ fail) #'i:lfail]
    
    [(_ (~and stx (~or (fallback . _)
                       (gather . _)
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

(define-syntax-rule
  (defrel/staged/fallback (name arg ...) body ...)
  (defrel/staged (name arg ...)
    (fallback (fresh () body ...))))

