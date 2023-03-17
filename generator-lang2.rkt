#lang racket/base

(provide
 quote
 cons
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

 defrel
 defrel-partial
 defrel/generator
 run

 (rename-out [g:generated-code generated-code])
 
 quasiquote
 (for-space mk quasiquote)
 unquote)

;; https://github.com/michaelballantyne/syntax-spec
(require syntax-spec
         (for-syntax racket/base
                     syntax/parse
                     racket/match
                     racket/list
                     syntax/id-set)
         
         (prefix-in g: "generator-lang.rkt")
         (only-in "staged-load.rkt" generate-staged invoke-staged))

(begin-for-syntax
  (struct runtime-rel [args-count] #:prefab)
  (struct generator-rel [args-count] #:prefab)
  (struct partial-rel [now-args-count later-args-count maybe-generator-rel] #:prefab)

  (define-persistent-symbol-table relation-info))

(syntax-spec
  (binding-class term-var)
  (binding-class relation-name)

  (extension-class term-macro
                   #:binding-space mk)

  (nonterminal quoted
    #:description "quoted value"
    n:number
    s:id
    ()
    (a:quoted . d:quoted))

  (nonterminal term
    #:description "miniKanren term"
    #:allow-extension term-macro
    
    (#%term-var x:term-var)
    ((~literal quote) t:quoted)
    ((~literal cons) t1:term t2:term)

    (~> x:id
        #'(#%term-var x))
    (~> n:number
        #'(quote n)))
  
  (nonterminal goal
    #:bind-literal-set goal-literals
    (== t1:term t2:term)
  
    (== v:term-var ((~datum partial-apply) rel:relation-name arg:term ...))
    (apply-partial rel:relation-name arg:term ...)

    (=/= t1:term t2:term)
    (absento t1:term t2:term)
    (symbolo t1:term)
    (numbero t1:term)
    (stringo t1:term)

    (fresh (x:term-var ...) g:goal ...+)
    #:binding {(bind x) g}
    
    (conde [g:goal ...+] ...+)
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
      (partial-rel (length (attribute now-arg)) (length (attribute later-arg))
                   (syntax-parse #'gen [#f #f] [g:id #'g])))
     #'r]
    #:rhs
    [#'(lambda (now-arg ... later-arg ...)
         (fresh ()
           (compile-runtime-goal g) ...))])

  (host-interface/expression
    (run n:racket-expr (q:term-var ...+) g:goal ...+)
    #:binding {(bind q) g}

    #'(g:run n (q ...) (compile-runtime-goal g) ...)))

(define-syntax compile-term
  (syntax-parser
    #:literals (#%term-var quote cons)
    [(_ (#%term-var x))
     #'x]
    [(_ (quote t))
     #'(quote t)]
    [(_ (cons t1 t2))
     #'(cons (compile-term t1) (compile-term t2))]))

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

  (define (free-vars goal-stx)
    (syntax-parse goal-stx
      #:literals (#%term-var fresh quote)
      [(#%term-var x) (immutable-free-id-set (list #'x))]
      [(quote _) (immutable-free-id-set)]
      [(fresh (x ...) g ...)
       (define body-vars
         (for/fold ([body-vars (immutable-free-id-set)])
                   ([g (attribute g)])
           (free-id-set-union body-vars (free-vars g))))
       (free-id-set-subtract
        body-vars
        (immutable-free-id-set (attribute x)))]
      [(a . d)
       (free-id-set-union (free-vars #'a) (free-vars #'d))]
      [_ (immutable-free-id-set)])))

(define-syntax compile-runtime-goal
  (syntax-parser
    #:literal-sets (goal-literals)

    [(_ (#%rel-app r:id arg ...))
     (match (symbol-table-ref relation-info #'r)
       [(runtime-rel arg-count)
        (when (not (= arg-count (length (attribute arg))))
          (raise-syntax-error #f "wrong number of arguments to relation" #'r))
        #'(r (compile-term arg) ...)]
       [_ (raise-syntax-error #f "runtime relation application expects relation defined by defrel" #'r)])]
    
    [(_ (== v:id ((~datum partial-apply) rel:id arg ...)))
     (match (symbol-table-ref relation-info #'rel)
       [(partial-rel now-args-count later-args-count maybe-generator-id)
        (when (not (= now-args-count (length (attribute arg))))
          (raise-syntax-error #f "wrong number of now-stage arguments to relation" #'r))
        (with-syntax ([rel-dyn #'rel]
                      [(later-placeholders ...) (make-list #'_ later-args-count)])
          #'(g:apply-reified v ((#f rel-dyn) (arg ...) (later-placeholders ...))))]
       [_ (raise-syntax-error #f "partial-apply expects relation defined by defrel-partial" #'r)])]
    
    #;[(apply-partial rel:relation-name arg:term ...)
       #''TODO]
    
    [(_ (constraint:binary-constraint t1 t2))
     #'(constraint.c (compile-term t1) (compile-term t2))]
    [(_ (constraint:unary-constraint t))
     #'(constraint.c (compile-term t))]
    
    [(_ (fresh (x:id ...) g ...))
     #'(g:fresh (x ...) (compile-runtime-goal g) ...)]
    [(_ (conde [g ...] ...))
     #'(g:conde [(compile-runtime-goal g) ...] ...)]
    [(_ (condg #:fallback gl ([x:id ...] [guard ...] [body ...]) ...))
     #'(g:condg (compile-runtime-goal gl)
                ([x ...] [(compile-runtime-goal guard) ...]
                         [(compile-runtime-goal body) ...]) ...)]
    [fail
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
    #;[(_ (#%rel-app r:id arg ...))
       ]
    #;[(_ (== v:id ((~datum partial-apply) rel:id arg ...)))
       ]
    #;[(apply-partial rel:relation-name arg:term ...)
       #''TODO]
    
    [(_ (constraint:binary-constraint t1 t2))
     #'(constraint.c (compile-term t1) (compile-term t2))]
    [(_ (constraint:unary-constraint t))
     #'(constraint.c (compile-term t))]
    
    [(_ (fresh (x:id ...) g ...))
     #'(g:fresh (x ...) (compile-now-goal g) ...)]
    [(_ (conde [g ...] ...))
     #'(g:conde [(compile-now-goal g) ...] ...)]
    [(_ (condg #:fallback gl ([x:id ...] [guard ...] [body ...]) ...))
     #'(g:condg (compile-now-goal gl)
                ([x ...] [(compile-now-goal guard) ...]
                         [(compile-now-goal body) ...]) ...)]
    [fail #'fail]

    [(_ (later g))
     #'(compile-later-goal g)]

    [(_ (~and stx (~or (staged . _)
                       (now . _))))
     (raise-syntax-error #f "not supported in generator code" #'stx)]    
    [_ (raise-syntax-error #f "unexpected goal syntax" this-syntax)]))

(define-syntax compile-later-goal
  (syntax-parser
    #:literal-sets (goal-literals)
    #;[(_ (#%rel-app r:id arg ...))
       ]
    #;[(_ (== v:id ((~datum partial-apply) rel:id arg ...)))
       ]
    #;[(apply-partial rel:relation-name arg:term ...)
       #''TODO]
        
    [(_ (constraint:binary-constraint t1 t2))
     #'(constraint.l (compile-term t1) (compile-term t2))]
    [(_ (constraint:unary-constraint t ))
     #'(constraint.l (compile-term t))]
    
    [(_ (fresh (x:id ...) g ...))
     #'(g:fresh (x ...) (compile-later-goal g) ...)]
    [(_ (conde [g ...] ...))
     #'(g:lconde [(compile-later-goal g) ...] ...)]
    [fail #'lfail]
    
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

