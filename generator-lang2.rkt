#lang racket/base

;; https://github.com/michaelballantyne/syntax-spec
(require syntax-spec
         (for-syntax racket/base syntax/parse)
         (prefix-in g: "generator-lang.rkt"))

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
    
    (~> x:id
        #'(#%term-var x))
    (#%term-var x:term-var)

    (~> n:number
        #'(quote n))
    ((~literal quote) t:quoted)
    ((~literal cons) t1:term t2:term))
  
  (nonterminal goal
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

    (later g:goal)
    (now g:goal)

    fail

    (~> (r:id arg ...)
        #'(#%rel-app r arg ...))
    (#%rel-app r:relation-name arg:term ...))

  (nonterminal condg-clause
    ([x:term-var ...] [guard:goal ...] [body:goal ...])
    #:binding {(bind x) guard body})

  
  (host-interface/definition
    (defrel (r:relation-name arg:term-var ...)
      g:goal ...+)
    #:binding [(export r) {(bind arg) g}]
    #:lhs
    [#'r]
    #:rhs
    [#''TODO])

  (host-interface/definition
    (defrel-partial
      (r:relation-name [now-arg:term-var ...+] [later-arg:term-var ...+])
      g:goal ...+)
    #:binding [(export r) {(bind now-arg later-arg) g}]
    #:lhs
    [#'r]
    #:rhs
    [#''TODO])

  (host-interface/definition
    (defrel/staged (r:relation-name arg:term-var ...)
      g:goal ...+)
    #:binding [(export r) {(bind arg) g}]
    #:lhs
    [#'r]
    #:rhs
    [#''TODO])
  
  (host-interface/definition
    (defrel-partial/staged
      (r:relation-name [now-arg:term-var ...+] [later-arg:term-var ...+])
      #:fallback f:goal
      g:goal ...+)
    #:binding [(export r) {(bind now-arg later-arg) f g}]
    #:lhs
    [#'r]
    #:rhs
    [#''TODO])

  (host-interface/expression
    (run n:racket-expr (q:term-var ...+) g:goal ...+)
    #:binding {(bind q) g}

    #''TODO)

  (host-interface/expression
    (run/staged n:racket-expr (q:term-var ...+) g:goal ...+)
    #:binding {(bind q) g}

    #''TODO))


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