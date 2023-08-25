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

(test
  (run* (v) (eval-ambo '(cons (amb 1 2) (amb 3 4)) v))
  '((1 . 3) (1 . 4) (2 . 3) (2 . 4)))
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
(test
  (run* (v) (staged (gen-eval-ambo '(amb 1 2) v)))
  '(1 2))
```

TODO: directly is weird

The specialized code directly has a `conde` for each case of the `amb`, without mentioning `amb`. The interpretive overhead has been specialized away.

TODO: generated-code hasn't been introduced. Maybe just show the generated code.

```
(test
  (generated-code)
  '(lambda (v7) (fresh (_.0) (== _.0 v7) (conde ((== '1 _.0)) ((== '2 _.0))))))
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
- `defrel/staged`
- `defrel/staged/fallback`

[Motivate with a program synthesis example. Do an aside of showing a program synthesis query in a more substantial interpreter.]

In a program synthesis example, we will often have a sketch of the program, along with holes representing the portions of the program to synthesize.
Staging can't help to do anything to optimize the holes, but it can specialize any computation involving the known sketch.
The full computation has to mix evaluation of staged generated code with evaluation of the holes using the unspecialized runtime interpreter.
The challenge is to construct both the interpreters and interactions between the generated code and the runtime interpreter.

```
(defrel/staged/fallback (name param ...)
  ...)
```

When we define a relation with `defrel/staged/fallback`, we generate both the staged and runtime version. We get the runtime version by removing the `later` annotations.

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
(defrel/staged/fallback (ms-eval-ambo e v) ;; only change
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
          ((ms-eval-ambo e2 v))))))))

(test
  (run* (v) (staged (ms-eval-ambo '(amb 1 2) v)))
  '(1 2))

(run 4 (e v) (staged (ms-eval-ambo `(cons (amb 1 2) ,e) v)))
```

### 3. let's scale that interpreter to support `lambda`

#### 3.1 a limited form of first-class relations (can be explained independently of staging)
We introduce the following forms:
- `defrel-partial`
- `partial-apply`
- `finish-apply`



#### 3.2 how to stage lambda
- `defrel-partial/staged`
- `specialize-partial-apply`  

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


Current grammar:

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
| (specialize-partial-apply t r t ...)

later goal lg := g(lg)

definition d :=
| (defrel (rname param ...) rg)
| (defrel/staged (rname param ...) sg)
| (defrel/staged/fallback (rname param ...) sg)
| (defrel-partial (rname tv [tv ...] [tv ...]) rg)
| (defrel-partial/staged (rname tv [tv ...] [tv ...]) sg)

expression e :=
| (run* (tv ...) rg)
| (run n (tv ...) rg)


Planned grammar:

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

