# The Staged miniKanren Language

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
  (run* (v) (eval-ambo '(amb 1 2) v))
  '(1 2))
```

## 1. write a staged interpreter that only staged fully ground programs

We introduce the following forms.
- `staged`
- `later`
- `defrel/generator`
- `gather`

We want to _stage_ the interpreter, specializing the interpreter to an expression in the first stage, and running the specialized miniKanren code in the second stage.
We want to define a generator `gen-eval-ambo`:

```
(defrel/generator (gen-eval-ambo e v)
 ...)
```

Which we can then use like this:

```
(test
  (run* (v) (staged (gen-eval-ambo '(amb 1 2) v)))
  '(1 2))
```

The specialized code directly has a `conde` for each case of the `amb`, without mentioning `amb`. The interpretive overhead has been specialized away.

```
(test
  (generated-code)
  '(lambda (v7) (fresh (_.0) (== _.0 v7) (conde ((== '1 _.0)) ((== '2 _.0))))))
```

What are the changes we need to think about when staging the interpreter? Why do we need to modify the body at all?

In relational staging, we are partitioning the computation into goals that should happen at staging-time and goals that should happen at runtime.

`later`: `(later <goal>)` defers the goal to the second stage (runtime). In a staged interpreter, we maintain the invariant that unifications with the resulting value is deferred to the second stage.

`gather`: How do we partition non-determinism? When we have a `conde`, do we want it to execute at staging-time or be part of the generated code? `(gather <goal>)` executes a goal, all branches within, and generates a runtime `conde` with a branch for each result of the goal.

```
(defrel/generator (gen-eval-ambo e v)
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
- `defrel/multistage`
- `defrel/multistage/fallback`

### 3. let's scale that interpreter to support `lambda`

#### 3.1 a limited form of first-class relations (can be explained independently of staging)
- `defrel-partial`
- `partial-apply`
- `finish-apply`

#### 3.2 how to stage lambda
- `defrel-partial/multistage`
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


grammar for plain goals:
term t
goal g :=
| (== t1 t2)
| (=/= t1 t2)
| etc.
| (fresh etc.)
| (conde etc.)
| (partial-apply t r t ...)
| (finish-apply t r t ...)

runtime goal rg :=
| g
| (staged sg)

staging-time goal sg :=
| g
| (later g)
| (gather sg)
| (specialize-partial-apply t r t ...)

multistage goal mg :=
| sg
| (fallback mg)
