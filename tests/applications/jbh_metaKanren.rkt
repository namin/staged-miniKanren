#|

TODOs:

- Try a more syntactic / unwalked term unification so as to get some specialization
  when the substitution is unknown.
- Reconsider whether each `later` is important. We can get away with more partial evaluation
  on values with the new fallback approach.
     Should we allow staging-time finish-apply?
- Try to figure out what goes wrong if we drop the `peanoo` constraints

|#

#|
author            : Bharathi Ramana Joshi
email             : joshibharathiramana@gmail.com
latest version at : http://github.com/iambrj/metaKanren
|#

#|
Syntax

<mk-program> ::= (run* <logic variable> <goal expr>) |
                 (run <peano> <logic variable> <goal expr>)

<goal expr> ::= (disj <goal expr> <goal expr>) |
                (conj <goal expr> <goal expr>) |
                (fresh (<logic-var>) <goal expr>) |
                (== <term expr> <term expr>) |
                (letrec-rel ((<lexical var> (<lexical var>*) <goal expr>)) <goal expr>) |
                (call-rel <lexical var> <term expr>*) |
                (delay <goal expr>)

<term expr> ::= (quote <datum>) |
                <lexical variable> |
                <logic variable> |
                (cons <term expr> <term expr>)

<datum> ::= <number> |
            <boolean> |
            <symbol> (not the var tag) |
            ()

<peano> ::= () |
            (<peano>)

<lexical variable> ::= <symbol>

<logic variable> ::= (var . <peano>)

<number> ::= [0-9]+

<boolean> ::= #f |
              #t
|#

#lang racket/base

(require "../../all.rkt")

(define empty-s '())
(define peano-zero '())
(define init-env '())

(define (peano n)
  (if (zero? n) '() `(,(peano (- n 1)))))

(defrel/staged/fallback (peanoo p)
  (conde
    [(== '() p)]
    [(fresh (p1)
       (== `(,p1) p)
       (peanoo p1))]))

(defrel/staged (var?o x)
  (fresh (val)
    (== `(var . ,val) x)
    (peanoo val)))

(defrel/staged (var=?o x y)
  (fresh (val)
    (== `(var . ,val) x)
    (== `(var . ,val) y)
    (peanoo val)))

(defrel/staged (var=/=o x y)
  (fresh (val1 val2)
    (== `(var . ,val1) x)
    (== `(var . ,val2) y)
    (=/= val1 val2)
    (peanoo val1)
    (peanoo val2)))

(defrel/staged (booleano b)
  (conde
    [(== #t b)]
    [(== #f b)]))

(defrel/staged (walko u s v)
  (conde
    [(== u v)
     (conde
       [(symbolo u) (== u v)]
       [(numbero u) (== u v)]
       [(booleano u) (== u v)]
       [(== '() u) (== u v)])]
    [(fresh (a d)
       (== `(,a . ,d) u)
       (=/= a 'var)
       (== u v))]
    [(var?o u)
     (conde
       [(== u v) (not-assp-subo u s)]
       [(fresh (pr-d)
          (assp-subo u s `(,u . ,pr-d))
          (walko pr-d s v))])]))

(defrel/staged/fallback (assp-subo v s out)
  (fresh (h t h-a h-d)
    (== `(,h . ,t) s)
    (== `(,h-a . ,h-d) h)
    (var?o v)
    (var?o h-a)
    (conde
      [(== h-a v) (== h out)]
      [(=/= h-a v) (assp-subo v t out)])))

(defrel/staged/fallback (not-assp-subo v s)
  (fresh ()
    (var?o v)
    (conde
      [(== '() s)]
      [(fresh (t h-a h-d)
        (== `((,h-a . ,h-d) . ,t) s)
        (var?o h-a)
        (=/= h-a v)
        (not-assp-subo v t))])))

(defrel/staged (ext-so u v s s1)
  (== `((,u . ,v) . ,s) s1))

;; I tried out making walko not a fallback relation. This means we have
;; some potential to specialize unifications. However, if any walk turns
;; out nondeterministically, we will fallback the unifyo call that uses it.
;; It might be possible to get better specialization than that by writing
;; a unify-syntactico that cases on the term expression rather than walked
;; term value first, and dispatches to unifyo only at term variables in the
;; expression.

; u, v <- {logic var, number, symbol, boolean, empty list, non-empty list}
; Total 36 + 5 (types match, but terms do not) = 41 cases
(defrel/staged/fallback (unifyo u-unwalked v-unwalked s s1)
  (fresh (u v)
    ;; The substitution will often be unknown, so these won't give
    ;; us a staging-time value for `u`.
    (walko u-unwalked s u)
    (walko v-unwalked s v)
    (conde
      [(var?o u) (var?o v) (var=?o u v) (== s s1)]
      [(var?o u) (var?o v) (var=/=o u v) (ext-so u v s s1)]
      [(var?o u) (numbero v) (ext-so u v s s1)]
      [(var?o u) (symbolo v) (ext-so u v s s1)]
      [(var?o u) (booleano v) (ext-so u v s s1)]
      [(var?o u) (== '() v) (ext-so u v s s1)]
      [(var?o u)
       (fresh (a d)
         (== `(,a . ,d) v)
         (=/= 'var a))
       (ext-so u v s s1)]
      [(numbero u) (var?o v) (ext-so v u s s1)]
      [(numbero u) (numbero v) (== u v) (== s s1)]
      [(numbero u) (numbero v) (=/= u v) (== #f s1)]
      [(numbero u) (symbolo v) (== #f s1)]
      [(numbero u) (booleano v) (== #f s1)]
      [(numbero u) (== '() v) (== #f s1)]
      [(numbero u)
       (fresh (a d)
         (== `(,a . ,d) v)
         (=/= 'var a))
       (== #f s1)]
      [(symbolo u) (var?o v) (ext-so v u s s1)]
      [(symbolo u) (numbero v) (== #f s1)]
      [(symbolo u) (symbolo v) (== u v) (== s s1)]
      [(symbolo u) (symbolo v) (=/= u v) (== #f s1)]
      [(symbolo u) (booleano v) (== #f s1)]
      [(symbolo u) (== '() v) (== #f s1)]
      [(symbolo u)
       (fresh (a d)
         (== `(,a . ,d) v)
         (=/= 'var a))
       (== #f s1)]
      [(booleano u) (var?o v) (ext-so v u s s1)]
      [(booleano u) (numbero v) (== #f s1)]
      [(booleano u) (symbolo v) (== #f s1)]
      [(booleano u) (booleano v) (== u v) (== s s1)]
      [(booleano u) (booleano v) (=/= u v) (== #f s1)]
      [(booleano u) (== '() v) (== #f s1)]
      [(booleano u)
       (fresh (a d)
         (== `(,a . ,d) v)
         (=/= 'var a))
       (== #f s1)]
      [(== '() u) (var?o v) (ext-so v u s s1)]
      [(== '() u) (numbero v) (== #f s1)]
      [(== '() u) (symbolo v) (== #f s1)]
      [(== '() u) (booleano v) (== #f s1)]
      [(== '() u) (== '() v) (== s s1)]
      [(== '() u)
       (fresh (a d)
         (== `(,a . ,d) v)
         (=/= 'var a))
       (== #f s1)]
      [(var?o v)
       (fresh (a d)
         (== `(,a . ,d) u)
         (=/= 'var a))
       (ext-so v u s s1)]
      [(numbero v)
       (fresh (a d)
         (== `(,a . ,d) u)
         (=/= 'var a))
       (== #f s1)]
      [(symbolo v)
       (fresh (a d)
         (== `(,a . ,d) u)
         (=/= 'var a))
       (== #f s1)]
      [(booleano v)
       (fresh (a d)
         (== `(,a . ,d) u)
         (=/= 'var a))
       (== #f s1)]
      [(== '() v)
       (fresh (a d)
         (== `(,a . ,d) u)
         (=/= 'var a))
       (== #f s1)]
      [(fresh (u-a u-d v-a v-d s-a)
         (== `(,u-a . ,u-d) u)
         (== `(,v-a . ,v-d) v)
         (=/= 'var u-a)
         (=/= 'var v-a)
         (conde
           [(== s-a #f) (== #f s1) (unifyo u-a v-a s s-a)]
           [(=/= s-a #f)
            (unifyo u-a v-a s s-a)
            (unifyo u-d v-d s-a s1)]))])))


(define mzero '())

(defrel/staged/fallback (mpluso $1 $2 $)
  (conde
    [(== '() $1) (== $2 $)]
    [(fresh (a d r1)
       (== `(,a . ,d) $1)
       (=/= 'delayed a)
       (== `(,a . ,r1) $)
       (mpluso d $2 r1))]
    [(fresh (d)
       (== `(delayed . ,d) $1)
       (== `(delayed mplus ,$1 ,$2) $))]))

(defrel/staged/fallback (bindo $ g2-rep $1)
  (conde
    [(== '() $) (== mzero $1)]
    [(fresh ($1-a $1-d v-a v-d)
       (== `(,$1-a . ,$1-d) $)
       (=/= 'delayed $1-a)
       (later (finish-apply g2-rep eval-bind-g2 $1-a v-a))
       (bindo $1-d g2-rep v-d)
       (mpluso v-a v-d $1))]
    [(fresh (d)
       (== `(delayed . ,d) $)
       (== `(delayed bind ,$ ,g2-rep) $1))]))

(defrel/staged/fallback (exto params args env env1)
  (conde
    [(== params '())
     (== args '())
     (== env env1)]
    [(fresh (x-a x-d v-a v-d)
       (== `(,x-a . ,x-d) params)
       (== `(,v-a . ,v-d) args)
       (exto x-d v-d `((,x-a . ,v-a) . ,env) env1))]))

(defrel/staged/fallback (lookupo x env v)
  (conde
    [(fresh (y u env1)
      (== `((,y . ,u) . ,env1) env)
      (conde
        [(== x y) (== v u)]
        [(=/= x y) (lookupo x env1 v)]))]))

(defrel/staged/fallback (not-in-envo x env)
  (conde
    [(== '() env)]
    [(fresh (y v env1)
       (== `((,y . ,v) . ,env1) env)
       (=/= x y)
       (not-in-envo x env1))]))

(defrel/staged/fallback (eval-args args env vals)
  (conde
    [(== args '())
     (== vals '())]
    [(fresh (a d va vd)
       (== `(,a . ,d) args)
       (== `(,va . ,vd) vals)
       (eval-texpro a env va)
       (eval-args d env vd))]))

(defrel-partial/staged (eval-bind-g2 rep [expr env] [s/c $])
  (eval-gexpro expr s/c env $))

(defrel/staged/fallback (eval-gexpro expr s/c env $)
  (conde
    [(fresh (ge)
       (== `(delay ,ge) expr)
       (== `(delayed eval ,ge ,s/c ,env) $))]
    [(fresh (ge1 ge2 ge1-$ ge2-$)
       (== `(disj ,ge1 ,ge2) expr)
       (eval-gexpro ge1 s/c env ge1-$)
       (eval-gexpro ge2 s/c env ge2-$)
       (mpluso ge1-$ ge2-$ $))]
    [(fresh (ge1 ge2 ge1-$ rep)
       (== `(conj ,ge1 ,ge2) expr)
       (eval-gexpro ge1 s/c env ge1-$)
       (specialize-partial-apply rep eval-bind-g2 ge2 env)
       (bindo ge1-$ rep $))]
    [(fresh (x ge s c env1)
       (== `(fresh (,x) ,ge) expr)
       (== `(,s . ,c) s/c)
       (exto `(,x) `((var . ,c)) env env1)
       (eval-gexpro ge `(,s . (,c)) env1 $))]
    [(fresh (te1 te2 v1 v2 s c s1)
       (== `(== ,te1 ,te2) expr)
       (== `(,s . ,c) s/c)
       ;; Staging-time evaluation does seem to get through these eval-texpros,
       ;; so we are getting term values at staging time.
       (eval-texpro te1 env v1)
       (eval-texpro te2 env v2)
       (gather
        (conde
          [(== #f s1) (== '() $)]
          [(=/= #f s1) (== `((,s1 . ,c)) $)]))
       (unifyo v1 v2 s s1))]
    [(fresh (id params geb ge e1 rep)
       (== `(letrec-rel ((,id ,params ,geb)) ,ge) expr)
       (== `((,id . (closr ,rep)) . ,env) e1)
       (specialize-partial-apply rep eval-closr id params geb env)
       (eval-gexpro ge s/c e1 $))]
    [(fresh (id args rep vargs)
       (== `(call-rel ,id . ,args) expr)
       (lookupo id env `(closr ,rep))
       (eval-args args env vargs)
       (later (finish-apply rep eval-closr vargs s/c $)))]))

(defrel-partial/staged (eval-closr rep [id params geb env] [vargs s/c $])
  (fresh (ext-env)
    (exto params vargs `((,id . (closr ,rep)) . ,env) ext-env)
    (eval-gexpro geb s/c ext-env $)))

(defrel/staged/fallback (eval-texpro expr env val)
  (conde
    [(== expr val)
     (conde
       [(numbero expr)]
       [(booleano expr)])]
    [(== `(quote ,val) expr)
     (not-in-envo 'quote env)
     (absento 'var val)
     (absento 'closr val)]
    [(symbolo expr)
     (lookupo expr env val)]
    [(fresh (e1 e2 v-e1 v-e2)
       (== `(cons ,e1 ,e2) expr)
       (== `(,v-e1 . ,v-e2) val)
       (not-in-envo 'cons env)
       (eval-texpro e1 env v-e1)
       (eval-texpro e2 env v-e2))]))

(defrel/staged/fallback (walk*o unwalked-v s u)
  (fresh (v)
    (walko unwalked-v s v)
    (conde
      [(== v u)
       (conde
         [(var?o v)]
         [(numbero v)]
         [(symbolo v)]
         [(booleano v)]
         [(== '() v)])]
      [(fresh (a d walk*-a walk*-d)
         (== `(,a . ,d) v)
         (=/= a 'var)
         (conde
           [(== '_. a)
            (== u v)]
           [(=/= '_. a)
            (== `(,walk*-a . ,walk*-d) u)
            (walk*o a s walk*-a)
            (walk*o d s walk*-d)]))])))

(defrel/staged/fallback (pullo $ $1)
  (conde
    [(== '() $) (== '() $1)]
    [(fresh (a d)
       (== `(,a . ,d) $)
       (== $ $1)
       (=/= 'delayed a))]
    [(fresh (ge s/c env $2)
       (== `(delayed eval ,ge ,s/c ,env) $)
       (eval-gexpro ge s/c env $2)
       (pullo $2 $1))]
    [(fresh ($a $b $a1 $2)
       (== `(delayed mplus ,$a ,$b) $)
       (pullo $a $a1)
       (mpluso $b $a1 $2)
       (pullo $2 $1))]
    [(fresh (saved-rep saved-$ saved-$1 $2)
       (== `(delayed bind ,saved-$ ,saved-rep) $)
       (pullo saved-$ saved-$1)
       (bindo saved-$1 saved-rep $2)
       (pullo $2 $1))]))

(defrel/staged/fallback (take-allo $ s/c*)
  (fresh ($1)
    (pullo $ $1)
    (conde
      [(== '() $1) (== '() s/c*)]
      [(fresh (a d-s/c* $d)
         (== `(,a . ,$d) $1)
         (== `(,a . ,d-s/c*) s/c*)
         (take-allo $d d-s/c*))])))

(defrel/staged/fallback (take-no n $ s/c*)
  (conde
    [(== '() n) (== '() s/c*)]
    [(=/= '() n)
     (fresh ($1)
       (pullo $ $1)
       (conde
         [(== '() $1) (== '() s/c*)]
         [(fresh (n-1 d-s/c* a d)
            (== `(,a . ,d) $1)
            (== `(,n-1) n)
            (== `(,a . ,d-s/c*) s/c*)
            (take-no n-1 d d-s/c*))]))]))

(defrel/staged/fallback (lengtho l len)
  (conde
    [(== '() l) (== '() len)]
    [(fresh (a d len-d)
       (== `(,a . ,d) l)
       (== `(,len-d) len)
       (lengtho d len-d))]))

(defrel/staged/fallback (reify-so v-unwalked s s1)
  (fresh (v)
    (walko v-unwalked s v)
    (conde
      [(var?o v)
       (fresh (len)
         (lengtho s len)
         (== `((,v . (_. . ,len)) . ,s) s1))]
      [(== s s1)
       (conde
         [(numbero v)]
         [(symbolo v)]
         [(booleano v)]
         [(== '() v)])]
      [(fresh (a d sa)
         (=/= 'var a)
         (== `(,a . ,d) v)
         (conde
           [(== '_. a)
            (== s s1)]
           [(=/= '_. a)
            (reify-so a s sa)
            (reify-so d sa s1)]))])))

(defrel/staged/fallback (reify-state/1st-varo s/c out)
  (fresh (s c v u)
    (== `(,s . ,c) s/c)
    (walk*o `(var . ()) s v)
    (reify-so v '() u)
    (walk*o v u out)))

(defrel/staged/fallback (reifyo s/c* out)
    (conde
      [(== '() s/c*) (== '() out)]
      [(fresh (a d va vd)
         (== `(,a . ,d) s/c*)
         (== `(,va . ,vd) out)
         (reify-state/1st-varo a va)
         (reifyo d vd))]))

(defrel/staged/fallback (eval-programo expr out)
  (conde
    [(fresh (lvar gexpr $ s/c*)
       (symbolo lvar)
       (== `(run* (,lvar) ,gexpr) expr)
       (eval-gexpro `(fresh (,lvar) ,gexpr) `(,empty-s . ,peano-zero) init-env $)
       (take-allo $ s/c*)
       (reifyo s/c* out))]
    [(fresh (n lvar gexpr $ s/c*)
       (symbolo lvar)
       (== `(run ,n (,lvar) ,gexpr) expr)
       (eval-gexpro `(fresh (,lvar) ,gexpr) `(,empty-s . ,peano-zero) init-env $)
       (take-no n $ s/c*)
       (reifyo s/c* out))]))

;; tests

;; eval-texpro of a partially known expression
;; produced a partially known value at staging time.
(run 0 (x q)
  (staged
   (fresh ()
     (eval-texpro `(cons ,x 1) '() q)
     (trace foo q))))
(generated-code)

;; This specializes, generating q == '()
(test
 (run 1 (q)
   (staged
    (unifyo '1 '1 '() q)))
 '(()))
(generated-code)

;; TODO: it seems like this unification should be able to specialize if
;; we didn't have peanoo enumerating variable names. But when I remove the
;; peanoo, mm 8 doesn't come back.
#;(time-test
 (run* (v q)
   (staged
    (unifyo `(var . ,v) '1 '() q)))
 '((_.0 (((var . _.0) . 1)))))
#;(generated-code)

;; The unification in this one specializes because we do
;; know the initial substitution.
(test
 (run 1 (q)
   (staged
    (eval-programo
     `(run* (res)
        (== 1 1))
     q)))
 ;; res is fresh
 '(((_.))))
(generated-code)

;; Here we don't know the substition for the second unification,
;; as it won't statically flow through the conjunction.
(test
 (run 1 (q)
   (staged
    (eval-programo
     `(run* (res)
        (conj (== 1 res) (== 2 res)))
     q)))
 ;; failure
 '(()))
(generated-code)

;; When specializing unify-2, we don't know what substitution
;; it will be called with. The current value-based unifier thus has
;; to just fall back.
(test
  (run 1 (res)
    (staged
     (eval-programo
      `(run* (z)
         (letrec-rel ((unify-2 (z) (== z '2)))
                     (call-rel unify-2 z)))
      res)))
  '((2)))
(generated-code)


(test
 (run* (x)
   (eval-programo
    `(run* (z)
       (letrec-rel ((incomplete-appendo (l1 l2 l)
                                        (disj
                                         (conj (== '() l1) (== l2 l))
                                         (== l 'recursive-case))))
                   (delay (call-rel incomplete-appendo '(1 2) '(3 4) z))))
    x))
 '((recursive-case)))
(generated-code)

;; Just printing generated code
(run 0 (x y w)
   (symbolo x)
   (symbolo y)
   (symbolo w)
   (staged
    (eval-programo
     `(run* (z)
        (letrec-rel ((appendo (l1 l2 l)
                              (disj
                               (conj (== '() l1) (== l2 l))
                               (fresh (a)
                                 (fresh (d)
                                   (fresh (l3)
                                     (conj (== (cons a d) l1)
                                           (conj (== (cons a l3) l)
                                                 (delay (call-rel appendo ,x
                                                                  ,y
                                                                  ,w))))))))))
                    (conj (call-rel appendo '(cat dog) '() '(cat dog))
                          (conj (call-rel appendo '(apple) '(peach) '(apple peach))
                                (call-rel appendo '(1 2) '(3 4) z)))))
     '((1 2 3 4)))))

(generated-code)


(begin


(test
  (run* (x)
    (eval-programo
     `(run* (z)
        (letrec-rel ((appendo (l1 l2 l)
                              (disj
                               (conj (== '() l1) (== l2 l))
                               (fresh (a)
                                 (fresh (d)
                                   (fresh (l3)
                                     (conj (== (cons a d) l1)
                                           (conj (== (cons a l3) l)
                                                 (delay (call-rel appendo d
                                                                  l2
                                                                  l3))))))))))
                    (call-rel appendo '(1 2) '(3 4) z)))
     x))
  '(((1 2 3 4))))

;; running-metakanren-query-to-appendo-relation-forward-works
(defrel (running-metakanren-query-to-appendo-relation-forward-works x)
  (staged
     (eval-programo
      `(run* (z)
         (letrec-rel ((appendo (l1 l2 l)
                               (disj
                                (conj (== '() l1) (== l2 l))
                                (fresh (a)
                                  (fresh (d)
                                    (fresh (l3)
                                      (conj (== (cons a d) l1)
                                            (conj (== (cons a l3) l)
                                                  (delay (call-rel appendo d
                                                                   l2
                                                                   l3))))))))))
                     (call-rel appendo '(1 2) '(3 4) z)))
      x)))

(test
  (run* (x)
    (running-metakanren-query-to-appendo-relation-forward-works x))
  '(((1 2 3 4))))


(test
  (run* (x)
    (eval-programo
     `(run* (z)
        (letrec-rel ((appendo (l1 l2 l)
                              (disj
                               (conj (== '() l1) (== l2 l))
                               (fresh (a)
                                 (fresh (d)
                                   (fresh (l3)
                                     (,x (== (cons a d) l1)
                                         (conj (== (cons a l3) l)
                                               (delay (call-rel appendo d
                                                                l2
                                                                l3))))))))))
                    (call-rel appendo '(1 2) '(3 4) '(1 2 3 4))))
     '((_.))))
  '(conj))


(defrel (synth-conj-in-appendo-relation-definition x)
  (staged
    (eval-programo
     `(run* (z)
        (letrec-rel ((appendo (l1 l2 l)
                              (disj
                               (conj (== '() l1) (== l2 l))
                               (fresh (a)
                                 (fresh (d)
                                   (fresh (l3)
                                     (,x (== (cons a d) l1)
                                         (conj (== (cons a l3) l)
                                               (delay (call-rel appendo d
                                                                l2
                                                                l3))))))))))
                    (call-rel appendo '(1 2) '(3 4) '(1 2 3 4))))
     '((_.)))))


(test
 (run* (x)
   (synth-conj-in-appendo-relation-definition x))
  '(conj))


(test
  (run 1 (x)
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                           (== 5 f)))
                    (call-rel five z)))
     x))
  '((5)))

;; demonstrates we can run the interpreter forward and get the answer
(defrel (run-interpreter-forward-get-answer-to-query x)
  (staged
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                           (== 5 f)))
                    (call-rel five z)))
     x)))

(test
 (run 1 (x)
   (run-interpreter-forward-get-answer-to-query x))
  '((5)))

; Don't get what we expect when all examples are internally ground
(test
  (run 1 (e1 e2)
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                           (== ,e1 ,e2)))
                    (call-rel five 5)))
     '((_.))))
  '(((_.0 _.0) $$ (num _.0))))

(defrel (synth-equation-parts-for-relation-call-to-constant-query-does-not-use-query-variable-one-answer e1 e2)
  (staged
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                           (== ,e1 ,e2)))
                    (call-rel five 5)))
     '((_.)))))

(test
 (run 1 (e1 e2)
   (synth-equation-parts-for-relation-call-to-constant-query-does-not-use-query-variable-one-answer e1 e2))
  '(((_.0 _.0) $$ (num _.0))))

;; Aha!
(test
  (run 1 (x)
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                           (== 7 7)))
                    (call-rel five 5)))
     x))
  '(((_.))))

(defrel (ensure-that-running-the-metakanren-query-produces-the-right-answer x)
  (staged
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                           (== 7 7)))
                    (call-rel five 5)))
     x)))

(test
 (run 1 (x)
   (ensure-that-running-the-metakanren-query-produces-the-right-answer x))
  '(((_.))))

(test
  (run 3 (e1 e2)
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                           (== ,e1 ,e2)))
                    (call-rel five 5)))
     '((_.))))
  '(((_.0 _.0) $$ (num _.0)) (#t #t) (5 f)))

(defrel (synth-equation-parts-for-relation-call-to-constant-query-does-not-use-query-variable-3-answers e1 e2)
  (staged
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                           (== ,e1 ,e2)))
                    (call-rel five 5)))
     '((_.)))))

(test
 (run 3 (e1 e2)
   (synth-equation-parts-for-relation-call-to-constant-query-does-not-use-query-variable-3-answers e1 e2))
  '(((_.0 _.0) $$ (num _.0)) (#t #t) (5 f)))

(test
  (run 1 (e1 e2)
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                           (== ,e1 ,e2)))
                    (call-rel five z)))
     '(5)))
  '((5 f)))

;; synth-values-for-equational-constraint-in-a relcall that uses the query variable.
(defrel (synth-values-for-equational-constraint-in-relcall e1 e2)
  (staged
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                           (== ,e1 ,e2)))
                    (call-rel five z)))
     '(5))))

(test
 (run 1 (e1 e2)
   (synth-values-for-equational-constraint-in-relcall e1 e2))
  '((5 f)))

; External grounding, extra examples to avoid overfitting, and with symbolo to
; fasten queries
(record-bench 'synth/ground-context 'unstaged 'metakanren #:description "synthesize the recursive call arguments in appendo")
(time-test
  (run 1 (relcall)
	(fresh (w x y)
		(symbolo w)
		(symbolo x)
		(symbolo y)
		(== relcall `(call-rel appendo ,w ,x ,y))
		(eval-programo
		 `(run* (z)
			(letrec-rel ((appendo (xs ys zs)
								  (disj
								   (conj (== '() xs) (== ys zs))
								   (fresh (a)
									 (fresh (d)
									   (fresh (res)
										 (conj (== (cons a d) xs)
											   (conj (== (cons a res) zs)
													 (delay ,relcall)))))))))
						(conj (call-rel appendo '(cat dog) '() '(cat dog))
							  (conj (call-rel appendo '(apple) '(peach) '(apple peach))
									(call-rel appendo '(1 2) '(3 4) z)))))
		 '((1 2 3 4)))))
  '((call-rel appendo d ys res)))

(record-bench 'synth/ground-context 'staging 'metakanren)
(defrel (synth-appendo-recursive-call relcall)
  (time-staged
    (eval-programo
     `(run* (z)
        (letrec-rel ((appendo (xs ys zs)
                              (disj
                               (conj (== '() xs) (== ys zs))
                               (fresh (a)
                                 (fresh (d)
                                   (fresh (res)
                                     (conj (== (cons a d) xs)
                                           (conj (== (cons a res) zs)
                                                 (delay ,relcall)))))))))
                    (conj (call-rel appendo '(cat dog) '() '(cat dog))
                          (conj (call-rel appendo '(apple) '(peach) '(apple peach))
                                (call-rel appendo '(1 2) '(3 4) z)))))
     '((1 2 3 4)))))

(record-bench 'synth/ground-context 'staged 'metakanren)
(time-test
 (run 1 (relcall)
   (fresh (w x y)
	 (symbolo w) (symbolo x) (symbolo y) 
	 (== relcall `(call-rel appendo ,w ,x ,y))
     (synth-appendo-recursive-call relcall)))
 '((call-rel appendo d ys res)))

; Thanks for the example, @bollu!
(test
  (run* (count)
    (eval-programo
     `(run ,count (z)
        (disj (== z 1)
              (== z 2)))
     '(1 2)))
  '(((())) (((_.0)) $$ (=/= ((_.0 ()))))))

;; Synthesizes the count of the run query; finds all possible solutions
(defrel (synth-count-of-run-query count)
  (staged
    (eval-programo
     `(run ,count (z)
        (disj (== z 1)
              (== z 2)))
     '(1 2))))

(test
 (run* (count)
   (synth-count-of-run-query count))
  '(((())) (((_.0)) $$ (=/= ((_.0 ()))))))

(test
  (run* (count answers)
    (eval-programo `(run ,count (z)
                      (disj (== z 1)
                            (== z 2)))
                   answers))
  '((() ()) ((()) (1)) (((())) (1 2)) ((((_.0)) (1 2)) $$ (=/= ((_.0 ()))))))

(defrel (synth-count-and-answers count answers)
  (staged
    (eval-programo `(run ,count (z)
                      (disj (== z 1)
                            (== z 2)))
                   answers)))

(test
 (run* (count answers)
   (synth-count-and-answers count answers))
  '((() ()) ((()) (1)) (((())) (1 2)) ((((_.0)) (1 2)) $$ (=/= ((_.0 ()))))))


; Gives disj in addition to conj
(define one (peano 1))
(test
 (run* (x)
   (eval-programo
    `(run ,one (z)
       (letrec-rel ((appendo (l1 l2 l)
                             (disj
                              (conj (== '() l1) (== l2 l))
                              (fresh (a)
                                (fresh (d)
                                  (fresh (l3)
                                    (,x (== (cons a d) l1)
                                        (conj (== (cons a l3) l)
                                              (delay (call-rel appendo d
                                                               l2
                                                               l3))))))))))
                   (call-rel appendo '(1 2) '(3 4) '(1 2 3 4))))
    '((_.))))
 '(disj conj))

(defrel (synth-appendo-portion x)
  (staged
    (eval-programo
     `(run ,one (z)
        (letrec-rel ((appendo (l1 l2 l)
                              (disj
                               (conj (== '() l1) (== l2 l))
                               (fresh (a)
                                 (fresh (d)
                                   (fresh (l3)
                                     (,x (== (cons a d) l1)
                                         (conj (== (cons a l3) l)
                                               (delay (call-rel appendo d
                                                                l2
                                                                l3))))))))))
                    (call-rel appendo '(1 2) '(3 4) '(1 2 3 4))))
     '((_.)))))

(test
 (run* (x)
   (synth-appendo-portion x))
 '(disj conj))

)
