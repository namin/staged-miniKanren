# The Staged miniKanren Language

## 0. write a vanilla interpreter

## 1. write a staged interpreter that only staged fully ground programs

- `staged`
- `later`
- `defrel/generator`
- `gather`

example interpreter with `list` and `amb`.

## 2. what if the program isn't fully ground?

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
