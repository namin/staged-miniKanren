# staged-miniKanren

This project explores multi-stage logic programming in miniKanren.
While partial evaluation has a rich history in both functional and logic programming,
multi-stage programming has so far only been explored in a functional/imperative setting,
with many success stories in high-performance computing.
How can we add staging constructs to a logic / relational programming language like miniKanren?
And what can we do with such constructs?
Can we turn an interpreter for a functional language written in miniKanren into a compiler translating any functional program into a relational one?
Can we go beyond and collapse towers of interpreters, covering hybrid paradigms?
What happens when a staged program is run "backwards"?
These are the sorts of questions we want to answer.

## Prototype

Our starting point is [the canonical miniKanren](https://github.com/miniKanren/miniKanren), which supports logical operators `==`, `fresh`, `conde`, interface operators `run`, `run*` and constraint operators `=/=`, `symbolo`, `numbero`, `absento`.

We extend miniKanren with staging operators `lift` and `lift-scope`. `lift` records some term for later, `lift-scope` takes a goal and a variable and runs the goal accumulating all lifted terms into the variable and discarding the local context.

### Tests

Our starting point is [a full interpreter in miniKanren for a subset of Racket](https://github.com/webyrd/faster-miniKanren/blob/master/full-interp.scm).
We [stage this interpreter](staged-interp.scm), and observe via [example runs](tests.scm) whether we can reasonably turn functional programs into relational programs without interpretation overhead.

We use the `lift` operator to defer unifications so that the generated program can perform all necessary constraints. We use `lift-scope` so that we can generate code for `if`/`conde` branches. We want to keep the generated relational code first-order, and so a good strategy is to not lift functions and applications. For recursively defined functions, we use explicit folding. For convenience, we change the interface of the interpreter so that the eval and lookup functions, `eval-expo` and `lookupo`, take an extra first parameter `stage?` which allows us to control whether we want/expect lifted or unlifted values.

Currently, we fix the scope of intermediary fresh variables after the fact... the current algorithm simply assigns a free variable to the closest enclosing fresh capturing all of its occurrences.

Voilà! This is enough to turn `append` into `appendo` automatically, turning the functional code into relational code.

### Generated Code: from `append` to `appendo`
 ```scheme
(gen 'append '(xs ys)
        '(if (null? xs) ys
             (cons (car xs)
                   (append (cdr xs) ys))))
;;=> equivalent to appendo: (define appendo (eval (gen 'append '(xs ys) ...)))
(lambda (xs ys out)
  (fresh (_.0)
    (== _.0 out)
    (letrec ([append (lambda (xs ys)
                       (lambda (_.1)
                         (fresh (_.2 _.3 _.4 _.5 _.6 _.9 _.7 _.10 _.11 _.13 _.8 _.12 _.14)
                           (== (cons _.2 '()) (cons _.3 '()))
                           (conde
                             ((== '() _.2) (== #t _.4))
                             ((=/= '() _.2) (== #f _.4)))
                           (== xs _.3)
                           (conde
                             ((=/= #f _.4) (== ys _.1))
                             ((== #f _.4)
                               (== (cons _.5 (cons _.6 '()))
                                   (cons _.7 (cons _.8 '())))
                               (== (cons _.5 _.6) _.1)
                               (== (cons (cons _.7 _.9) '())
                                   (cons _.10 '()))
                               (== xs _.10)
                               (== (cons (cons _.11 _.12) '())
                                   (cons _.13 '()))
                               (== xs _.13) (== ys _.14)
                               ((append _.12 _.14) _.8))))))])
      (fresh (_.15 _.16)
        (== xs _.15)
        (== ys _.16)
        ((append _.15 _.16) _.0)))))
```

## Next Steps

- The generated code could be optimized (e.g. exploding unification between two concrete pairs, shortcutting intermediary unifications, ...).
- Can we be more principled in generating code and fixing scopes?
- Can we collapse towers of interpreters? What would be interesting towers to try to collapse?
- Can we meta-run backwards? That is, can we run the staged interpreter with a partially known expression or partially known output? What about constraints like quines?
- What about JIT? Would it make sense to compile/optimize based on static = ground and dynamic = unknown/variable? Would this be similar to partial reduction?
- Use case: can we optimize the query to generate quines to get a quine generator without interpretive overhead? In this case, the binding annotations will have to fluidly follow the query?
- Use case: Can we optimize a mostly ground interpreter to fully run examples where the holes are irrelevant and only defer (or possibly synthesize, if given test cases) what it does not know? How can any staging help at all?
- Question: Don't we need to keep the constraints from the meta-run when fulfilling the run?
- Question: Meta-running the staged interpreter which assumes a ground expression causes lots of hypotheticals that are easily disproved when considering the deferred operations. Can we fail fast and still package an optimized deferred run?
