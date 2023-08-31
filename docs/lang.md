# The Staged miniKanren Language

## Preview of the purpose of staging

## 0. write a vanilla interpreter

[Assume the reader knows about writing interpreters.]
We're starting with an interpreter that supports numbers, `cons`, and non-determinism.

```
(defrel (eval-ambo e v)
  (conde
    ((numbero e)
     (== e v))
    ((fresh (e1 e2 v1 v2)
       (== e `(cons ,e1 ,e2))
       (== v (cons v1 v2))
       (eval-ambo e1 v1)
       (eval-ambo e2 v2)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (conde
         ((eval-ambo e1 v))
         ((eval-ambo e2 v)))))))

(run* (v) (eval-ambo '(cons (amb 1 2) (amb 3 4)) v))
;; => ((1 . 3) (1 . 4) (2 . 3) (2 . 4))
```

TODO: consider if-null instead of amb?

## 1. write a staged interpreter that only stages fully-ground programs

TODO: this introduction is more helpful for us than for the reader. Consider replacing with the grammar extension.

We introduce the following forms.
- `staged`
- `later`
- `defrel/staged`
- `gather`

We want to _stage_ the interpreter: specializing the interpreter `eval-ambo` with respect to an expression `e` (the first argument of the interpreter) in the first stage. The result of specialization is miniKanren code, without the interpretive overhead of the interpreter (for example, the interpreter loop has gone away). In the second stage, we run the generated miniKanren code.

We want to define a generator `gen-eval-ambo` for our staged intepreter:

```
(defrel/staged (gen-eval-ambo e v)
 ...)
```

Which we can then use like this:

```
(run* (v) (staged (gen-eval-ambo '(amb 1 2) v)))
;; => (1 2)
```

TODO: directly is weird

The specialized code directly has a `conde` for each case of the `amb`, without mentioning `amb`. The interpretive overhead has been specialized away.

TODO: generated-code hasn't been introduced. Maybe just show the generated code.

```
(generated-code)
;; => (lambda (v7) (fresh (_.0) (== _.0 v7) (conde ((== '1 _.0)) ((== '2 _.0)))))
```

TODO: Modulo some extra lambdas and freshes:

```
(run* (v) (conde ((== 1 v)) ((== 2 v))))
```

What are the changes we need to think about when staging the interpreter? Why do we need to modify the body at all?

In relational staging, we are partitioning the computation into goals that should happen at staging-time and goals that should happen at runtime.

`later`: `(later <goal>)` defers the goal to the second stage (runtime). In a staged interpreter, we maintain the invariant that unifications with the resulting value (the `v` argument of the interpreter) are deferred to the second stage.

`gather`: How do we partition non-determinism? When we have a `conde`, do we want it to execute at staging-time or be part of the generated code? `(gather <goal>)` executes a goal, all branches within, and generates a runtime `conde` with a branch for each result of the goal.

```
(defrel/staged (gen-eval-ambo e v)
  (conde
    ((numbero e)
     (later (== e v))) ;; later is new
    ((fresh (e1 e2 v1 v2)
       (== e `(cons ,e1 ,e2))
       (later (== v (cons v1 v2))) ;; later is new
       (gen-eval-ambo e1 v1)
       (gen-eval-ambo e2 v2)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (gather ;; gather is new
        (conde
          ((gen-eval-ambo e1 v))
          ((gen-eval-ambo e2 v))))))))
```

To elaborate, the `(staged <goal>)` form from the example takes a goal that runs at staging-time.
It does both steps of running the first stage, and the second stage resulting from running the first stage.

An issue with this interpreter is that the staging phase is sometimes non-deterministic:
```
(run 2 (e v) (staged (gen-eval-ambo `(cons (amb 1 2) ,e) v)))
```
It doesn't work if the program is not fully known.

## 2. what if the program isn't fully ground?
We introduce the following forms:

- `fallback`

[Motivate with a program synthesis example. Do an aside of showing a program synthesis query in a more substantial interpreter.]

In a program synthesis example, we will often have a sketch of the program, along with holes representing the portions of the program to synthesize.
Staging can't help to do anything to optimize the holes, but it can specialize any computation involving the known sketch.
The full computation has to mix evaluation of staged generated code with evaluation of the holes using the unspecialized runtime interpreter.
The challenge is to construct both the interpreters and interactions between the generated code and the runtime interpreter.

```
(defrel/staged (name param ...)
  ...)
```

When we define a relation with `defrel/staged`, we generate both the staged and runtime version. We get the runtime version by removing the `later` annotations.

Here the runtime version of `ms-eval-ambo` would be identical to the `eval-ambo` relation we saw in section 1.

For this query,

```
(run 4 (e v) (staged (ms-eval-ambo `(cons (amb 1 2) ,e) v)))
```

We would like to produce generated code like this:

```
(run  4 (e v)
  (fresh (a d)
    (== v (cons a d))
    (conde ((== a 1)) ((== a 2)))
    (ms-eval-ambo/runtime e d)))
```

How do we decide at staging-time that we've encountered a hole and don't have enough information for staging?
Our approach is to try evaluating the generator interpreter and see if it's non-deterministic.
If it is, generate a fallback to the runtime version (which is itself automatically generated).

Notice that in the example above, the first clause of the `cons`, `'(amb 1 2)`, is fully known and produces specialized code, while the second clause, `e`, is unknown and generates a fallback to the runtime.

```
(defrel/staged (ms-eval-ambo e v)
  (fallback  ;; only change
   (conde
     ((numbero e)
      (later (== e v)))
     ((fresh (e1 e2 v1 v2)
        (== e `(cons ,e1 ,e2))
        (later (== v (cons v1 v2)))
        (ms-eval-ambo e1 v1)
        (ms-eval-ambo e2 v2)))
     ((fresh (e1 e2)
        (== e `(amb ,e1 ,e2))
        (gather
         (conde
           ((ms-eval-ambo e1 v))
           ((ms-eval-ambo e2 v)))))))))

(test
  (run* (v) (staged (ms-eval-ambo '(amb 1 2) v)))
  '(1 2))

(run 4 (e v) (staged (ms-eval-ambo `(cons (amb 1 2) ,e) v)))
```

### 3. let's scale that interpreter to support `lambda`

Control flow means that any given piece of code may execute more than once, and when staging, we want to ensure that we generate each piece of code only once.
Accomplishing this will require us to add a new staging mechanism to our system, specifically some form of first-class code in the staging language.

As an example, we want to add function abstraction and application as expressions in our interpreter.
So we will add expressions: `(lambda (x) e)` for function abstraction and `(e e)` for application.

We want to generate code for an abstraction once, and then use it each time the abstraction is applied.
Almost any control flow structure is going to need this feature of generating once, using many times.

Our solution is to introduce the mechanism of partially applicable relations.
An initial partial application of the relation supplies some arguments and creates a first-class representation of code as a miniKanren term.
A final application supplies the remaining arguments.

In the context of staging, code is generated at the partial application and reused at each final application.

#### 3.1 a limited form of first-class relations (can be explained independently of staging)
We introduce the following forms:
- `defrel-partial`
- `partial-apply`
- `finish-apply`

We first introduce some forms for partial application independently of staging starting with our vanilla interpreter. In the next section, we will see how this augmented interpreter can be staged.

Our interpreter for this section is `eval-lambda-ambo` and it takes an extra argument for the environment mapping variables to values.
`(defrel (eval-lambda-ambo e env v)
  ...)`

Here is an example of running the interpreter:

```
(run* (v) (eval-lambda-ambo '((lambda (x) (amb x 3)) (amb 1 2)) '() v))
;; => (3 3 1 2)
```

We will use a partial applicable relation to represent a closure.
When evaluating the `lambda`, we will partially apply this relation, having the parameter, body and environment.
When applying the closure, we will finish the partial application with the argument and returned value.

We use `defrel-partial` to define a partially applicable relation.

```
(defrel-partial (apply-lambda-ambo rep [x e env] [arg v])
  (eval-lambda-ambo e (cons (cons x arg) env) v))
```

This relation takes the parameter, body and birth environment of the closure as the arguments for the partial application.
When finishing the application, the relation takes the argument and the returned value (as an "output" parameter of the relation).

The relation body here evaluates the body of the closure in an extended environment.

When creating a closure, the interpreter partially applies this new relation, using `partial-apply`. The first argument unifies with the first-class representation. The second argument is the relation name. The remainder are the first-application arguments.

```
...
    ((fresh (x e0)
       (== e `(lambda (,x) ,e0))
       (partial-apply v apply-lambda-ambo x e0 env)))
....
```

When applying a closure, we `finish-apply` this relation. The first argument is the first-class representation, the second argument is the relation name, and the remaining arguments are the arguments to finish the application.

```
...
    ((fresh (e1 e2 v1 v2)
       (== e `(,e1 ,e2))
       (eval-lambda-ambo e1 env v1)
       (eval-lambda-ambo e2 env v2)
       (finish-apply v1 apply-lambda-ambo v2 v)))))
...
```

Note that we have a case for variables in the interpreter.

```
...
    ((symbolo e)
     (lookupo e env v))
...
```

We use an auxiliary relation `lookupo` to lookup the value for a variable in the environment.

```
(defrel (lookupo x env v)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
     [(== x y) (== v b)]
     [(=/= x y) (lookupo x rest v)])))
```

[Here is the full code:

```
(defrel (lookupo x env v)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
     [(== x y) (== v b)]
     [(=/= x y) (lookupo x rest v)])))

(defrel-partial (apply-lambda-ambo rep [x e env] [arg v])
  (eval-lambda-ambo e (cons (cons x arg) env) v))

(defrel (eval-lambda-ambo e env v)
  (conde
    ((numbero e)
     (== e v))
    ((fresh (e1 e2 v1 v2)
       (== e `(cons ,e1 ,e2))
       (== v (cons v1 v2))
       (eval-lambda-ambo e1 env v1)
       (eval-lambda-ambo e2 env v2)))
    ((fresh (e1 e2)
       (== e `(amb ,e1 ,e2))
       (conde
         ((eval-lambda-ambo e1 env v))
         ((eval-lambda-ambo e2 env v)))))
    ((symbolo e)
     (lookupo e env v))
    ((fresh (x e0)
       (== e `(lambda (,x) ,e0))
       (partial-apply v apply-lambda-ambo x e0 env)))
    ((fresh (e1 e2 v1 v2)
       (== e `(,e1 ,e2))
       (eval-lambda-ambo e1 env v1)
       (eval-lambda-ambo e2 env v2)
       (finish-apply v1 apply-lambda-ambo v2 v)))))
```
]

#### 3.2 how to stage lambda
We introduce the following forms:
- `defrel-partial/staged`
- `specialize-partial-apply`

[TODO: Change ms- to s-.]

```
;; change here
(defrel/staged (ms-lookupo x env v)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (fallback
     (conde
       [(== x y) (== v b)]
       [(=/= x y) (ms-lookupo x rest v)]))))

;; change here
(defrel-partial/staged (ms-apply-lambda-ambo rep [x e env] [arg v])
  (ms-eval-lambda-ambo e (cons (cons x arg) env) v))

(defrel/staged (ms-eval-lambda-ambo e env v)
  (fallback
   (conde
     ((numbero e)
      (later (== e v)))
     ((fresh (e1 e2 v1 v2)
        (== e `(cons ,e1 ,e2))
        (later (== v (cons v1 v2)))
        (ms-eval-lambda-ambo e1 env v1)
        (ms-eval-lambda-ambo e2 env v2)))
     ((fresh (e1 e2)
        (== e `(amb ,e1 ,e2))
        (gather
         (conde
           ((ms-eval-lambda-ambo e1 env v))
           ((ms-eval-lambda-ambo e2 env v))))))
     ((fresh (env-v)
        (symbolo e)
        ;; We want to make the unifications with v later-stage.
        ;; We put it before the lookupo to make sure the runtime version knows the value.
        (later (== v env-v))
        (ms-lookupo e env env-v)))
     ((fresh (x e0)
        (== e `(lambda (,x) ,e0))
        ;; change here
        (specialize-partial-apply v ms-apply-lambda-ambo x e0 env)))
     ((fresh (e1 e2 v1 v2)
        (== e `(,e1 ,e2))
        (ms-eval-lambda-ambo e1 env v1)
        (ms-eval-lambda-ambo e2 env v2)
        ;; change here
        (later (finish-apply v1 ms-apply-lambda-ambo v2 v)))))))
```

It's possible to construct a query where you reach the application case, but you have a fresh logic variable for the first-class representation of the partial application. In this case, we have to fallback to invoking the runtime version of the partially applicable relation with the first-application arguments being fresh logic variables.

## Language

multistage: we assume it's possible to reach an application without knowing the lambda.
Still doesn't happen for fully ground.

standard miniKanren is the base language that we start form.
we add partial applications.

runtime
- can use `staged`
- cannot use `later`

later
- cannot use `later`
- cannot use `stage`

staging-time
- can use `later`
- can use `gather`
- can use `specialize-partial-apply`
- cannot use `staged`

multistage (like staging-time)
is an extension of staging-time
- can use `fallback`


term var tv
term t

goal g(p) :=
| (== t1 t2)
| (=/= t1 t2)
| etc.
| (fresh (tv ...) p ...)
| (conde (p ...) ...)
| (partial-apply t rname t ...)
| (finish-apply t rname t ...)

runtime goal rg :=
| g(rg)
| (staged sg)

staging-time goal sg :=
| g(sg)
| (later lg)
| (gather sg)
| (fallback sg)
| (specialize-partial-apply t r t ...)

later goal lg := g(lg)

definition d :=
| (defrel (rname param ...) rg)
| (defrel/staged (rname param ...) sg)
| (defrel-partial (rname tv [tv ...] [tv ...]) rg)
| (defrel-partial/staged (rname tv [tv ...] [tv ...]) sg)

expression e :=
| (run* (tv ...) rg)
| (run n (tv ...) rg)

