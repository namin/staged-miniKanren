# Multi-stage miniKanren

## Abstract

We introduce multi-stage miniKanren, which augments miniKanren with staging constructs that allow us to have precise control over the staging process. We have novel constructs to account for non-determinism. We use multi-stage miniKanren to stage interpreters written as relations, in which the programs under interpretation can contain holes representing unknown values. We apply this staging framework to a relational interpreter for a subset of Racket, and demonstrate significant performance gains across multiple synthesis problems.

## Getting started with Docker (optional)

The docker image can be pulled with `git pull namin/staged-minikanren` or build here with `docker build --platform=linux/amd64 -t namin/staged-minikanren .`.

Note: on Mac, _disable_ Rosetta in the Docker Desktop settings. [See this thread.](https://racket.discourse.group/t/racket-docker-m1-rosetta/2947/6)

An interactive Racket session can be run with `docker run -it namin/staged-minikanren`. Try a sanity check with the following lines:
```
> (require "all.rkt")
> (run 1 (q) (staged (== q 'fun)))
'(fun)
```

For a larger example, see [small-interp](small-interp).

The tests and benchmarks should run with `docker run -i namin/staged-minikanren` and these additional arguments:
- `racket tests/all.rkt`
- `./benchrun.sh`

You can copy the benchmark table out of the docker container with `docker cp CONTAINER_ID:/app/staged-miniKanren/bench-results/bench_paper.tex .`, where the `CONTAINER_ID` can be obtained by inspecting `docker ps -a`.

## Dependencies

Multi-stage miniKanren is a [Racket](https://racket-lang.org/) library which relies on two existing pieces of software: [syntax-spec-v2](https://pkgs.racket-lang.org/package/syntax-spec-v2), a metalanguage for implementing DSLs in Racket; and a modified version of [faster-minikanren](https://github.com/michaelballantyne/faster-minikanren), an efficient implementation of miniKanren with constraint solving. The former dependency can be installed using Racket's builtin package manager, [raco](https://docs.racket-lang.org/raco/index.html), by running the shell command:

```sh
raco pkg install syntax-spec-v2
```

The latter dependency is bundled with this project, under `private/faster-minikanren`, as a git submodule. To install this, use the following shell commands:

```sh
git submodule init
git submodule update
```

To build the plots used in the paper, an additional dependency, [`plot`: the racket plotting library](https://docs.racket-lang.org/plot/), is required. This can be installed by running the shell command:

```sh
raco pkg install plot
```

## Tests and Examples

Tests for the project are available under the [`tests/`](./tests/) directory. A test file can be run individually by running `racket FILE_TO_TEST`. Running the file [`tests/all.rkt`](./tests/all.rkt) will run the full test suite.

### Relational Interpreters

There are a number of relational interpreters for different languages in this project. The largest interpreter is for the subset of Racket used in prior papers to demonstrate relational synthesis. The version of this interpreter without annotations is in [`unstaged-interp.scm`](./unstaged-interp.scm), and the version with staging annotations is in [`staged-interp.scm`](./staged-interp.scm). Interpreters for a smaller dialect of Racket are present in the [`small-interp`](./small-interp.scm) directory. The relational interpreter for the λ-or language from Figure 8 of the paper is defined in [`tests/interp-doc.rkt`](./tests/interp-doc.rkt).

### Case Studies

The case studies presented in section 6.1 of the paper are present in the [`tests/applications/`](./tests/applications) sub-directory. These include:
- [Parsing with Derivatives](./tests/applications/parsing-with-derivatives.rkt)
- [Theorem Checker Turned Prover](./tests/applications/proof.rkt)
- [Negation Normal Form (NNF)](./tests/applications/dl.rkt)
- [Peano](./tests/applications/peano-fib.rkt)
- [Synthesis](./tests/doc-bench.rkt)
- [Double Evaluators](./tests/applications/double-eval.rkt)
- [miniKanren-in-miniKanren](./tests/applications/metamk.rkt)
- [Grammar Parsers](./tests/applications/grammars.rkt)

### Plots

The benchmarking suite includes the ability to create line plots, demonstrating the speedup staging provides for various input sizes. These plots are generated by running [`tests/graphs.rkt`](./tests/graphs.rkt), which will produce three SVG files in the `./bench-results` sub-directory. These plots were not included in the paper due to space constraints.

## Reusability Guide

In this short tutorial, we give an overview of the multi-stage miniKanren language and how to build programs using it. We assume the reader is familiar with relational programming in miniKanren and functional staging.

### Installing Multi-Stage miniKanren as a Racket Package

To use multi-stage miniKanren as a dependency in other racket projects, it must be installed as a racket package. This can be done with the following shell command:

```sh
raco pkg install
```

### Writing a Simple Multi-Stage miniKanren Program

Once the package is installed, we can use it to build new multi-stage miniKanren programs. Write the following to a file named `example.rkt`:

```scheme
#lang racket

(require staged-miniKanren)

(defrel (same x y)
  (== x y))

(run* (p) (same p p))
```

Running this program with `racket ./example.rkt` should give the result `'(_.0)`.

### Features of Multi-Stage miniKanren

#### Staging-Time and Run-Time

As with traditional two-stage functional programming, staged relational programming separates goal execution into a *staging-time stage* that generates goal code for run-time, and a *run-time stage* in which those generated goals, together with goals not executed at staging-time, are subsequently run in a query. In this model the two stages are separate and disjoint, with the staging-time component wholly preceding the run-time component. Portions of code for each stage can be inter-nested, and code executed at run time can contain fragments generated at staging time within it.

We can promote run-time code into staging-time code using the `staged` operator. Similarly, we can defer the execution of some staging-time code to runtime using the `later` operator. Both of these operators are provided by the `staged-miniKanren` library. We can define code as available both at staging-time and runtime using the `defrel/staged` declaration. The following code demonstrates these:

```scheme
#lang racket

(require staged-miniKanren)

(defrel/staged (zooo p r q)
  (== q (list p r))
  (== p 'dog)
  (later ; Delays the following code back to run-time
    (== r 'fish)))

(run 1 (q)
  (staged ; Moves the following code to staging-time
    (fresh (p r)
      (zooo p  r q))))

(generated-code)
```

Running this file gives us two answers back: the result of the program's execution and the code which was generated at staging-time (which we access using the `generated-code` procedure).

```
'((dog fish))
'(lambda (q1) (fresh (temp1) (== (list 'dog temp1) q1) (== temp1 'fish)))
```

The result of the `run` expression is that of traditional miniKanren without annotations. This "erasure property" is an valuable feature of multi-stage miniKanren: staging annotations do not affect the results of the computation. If we look at the generated code, we can see that multi-stage miniKanren was able to perform the `(== p 'dog)` unification at staging-time, replacing all instances of the `p` variable with the ground symbol `'dog`. As we asked, it delayed the `(== r 'fish)` unification to runtime (despite the variable having been renamed). However, because the unification `(== q (list p r))` requires the runtime value `r`, this unification was also delayed to runtime.

#### Staging-Time Non-Determinism

One important property of multi-stage miniKanren is that code run at staging-time must produce exactly one result: the goal of staging is to produce a single residual program to be executed at run-time. This is not to say that multi-stage miniKanren programmers cannot use nondeterministic search at staging-time, only that they must annotate how it should be handled using either a `fallback` or `gather` annotation.

Marking a goal with the `fallback` annotation instructs multi-stage miniKanren to attempt to execute the goal deterministically at staging-time, but to defer to run-time if that isn't possible. If the goal should succeed deterministically, then its result of that nondeterministic success is spliced into the residual program. If multiple answers are returned, then the `fallback`-annotated goal will "falls back" to runtime, splicing the goal itself into the residual program, as if the `fallback` annotation had been replaced with `later`.

The `gather` annotation marks a goal as one which will produce a finite number of results and that all possible results of the goal's execution should be computed at staging-time. Each of these possible results are then encoded as a disjunction in the residual program. 

The following example program demonstrates this difference:

```scheme
#lang racket

(require staged-miniKanren)

(defrel/staged (noto p q)
  (conde
    [(== p #t) (== q #f)]
    [(== p #f) (== q #t)]))

(run 1 (p q) (staged (fallback (noto p q))))
(generated-code)

(run 1 (p q) (staged (gather (noto p q))))
(generated-code)
```

Running the above program gives the following four results - the result of executing `(noto p q)` goal with the `fallback` annotation, the code generated by this execution, the result of executing `(noto p q)` with the `gather` annotation, and the code generated by this later execution:

```
'((#t #f))
'(lambda (p1 q2)
   (fresh (temp7 temp8)
     (== temp7 p1)
     (== temp8 q2)
     (invoke-fallback staged/1 temp7 temp8)))
'((#t #f))
'(lambda (p7 q8)
   (fresh (temp9 temp10)
     (== temp9 p7)
     (== temp10 q8)
     (disj
      (fresh () (== temp9 '#t) (== temp10 '#f))
      (fresh () (== temp9 '#f) (== temp10 '#t)))))
```

The results of either execution are the same, as to be expected by the erasure property of multi-stage miniKanren. The code of the `fallback`-annotated goal deferred the sub-goal to runtime, as the goal `(noto p q)` is non-deterministic. The generated goal `(invoke-fallback staged/1 ...)` is the invocation of a version of the `noto` relation without staging annotations. The code of the `gather`-annotated goal is a disjunction over both nondeterministic possibilities which result from executing the `noto` relation.

#### Partial Relation Applications and Specialization

Staged programming has the ability to inline calls to both relations and functions, which is instrumental for staging out potential overhead, but may cause code explosion as too many relations/functions are inlined. To prevent this, functional staging systems allow generated code to contain function objects created using λ expressions. The relational programming analogue of this would be higher-order relations, a limited form of which are provided by multi-stage miniKanren. 

Multi-stage miniKanren provides partial relations: relations with two sets of arguments: one set available at staging-time, and the other available at runtime. The `defrel-partial/staged` declaration defines such relations, the `specialize-partial-apply` form supplies the set of staging-time parameters, and the `finish-apply` form provides the runtime parameters. Both `specialize-partial-apply` must statically know which relation is being applied in order to avoid higher-order unification. To demonstrate these, consider the following program, which defines a partial relation for checking if a runtime object against a list of staging-time objects and uses that relation to check if an object is a vowel:

```scheme
#lang racket

(require staged-miniKanren)

(defrel/staged (membero elem choices)
  (fresh (h t)
    (== choices (cons h t))
    (conde
     [(== h elem)]
     [(membero elem t)])))

(defrel-partial/staged (membero-staged rep [choices] [elem])
  (gather (membero elem choices)))

(run* (p)
  (staged
   (fresh (vowelo)
     (specialize-partial-apply vowelo membero-staged '(a e i o u))
     (later (finish-apply vowelo membero-staged 'a))
     (later (finish-apply vowelo membero-staged p)))))

(generated-code)
```

This program produces the following two results:

```
'(a e i o u)
'(lambda (p3)
   (fresh (temp72 temp73)
     (== temp72 p3)
     (==
      temp73
      (apply-rep
       'membero-staged
       '((a e i o u))
       (lambda (_6)
         (fresh
          (temp71)
          (== temp71 _6)
          (disj
           (fresh () (== temp71 'a))
           (fresh () (== temp71 'e))
           (fresh () (== temp71 'i))
           (fresh () (== temp71 'o))
           (fresh () (== temp71 'u)))))))
     (finish-apply temp73 (membero-staged (_) ('a)))
     (finish-apply temp73 (membero-staged (_) (temp72)))))
```

The first result (the return value of the `run*` expression) is as expected. The second result (the generated code) shows how a higher-order relation object is created and applied. The specialized version of the code (which has eliminated any reference to the original item list) is the `lambda` expression present in the call to `apply-rep`. The call to `apply-rep` creates a higher-order relation object which encodes both the original relation's name, the arguments provided to it, and the resulting specialized relation.

## Summary of Forms

- `(defrel/staged (NAME ARG ...) BODY ...)`: Defines a relation called `NAME` both at staging-time and run-time. The resulting relation is the result of `(conj BODY ...)` parameterized by over `ARG ...`. The erased semantics are identical to that of traditional miniKanren's `defrel`.
- `(staged GOAL)`: Annotation which marks that a goal should be executed at staging-time. The goal must be deterministic (produce exactly one result) or an error will be thrown. The erased semantics are identical to that of `GOAL`.
- `(later GOAL)`: Annotation which marks that a staged goal should be delayed back to runtime. The erased semantics are identical to that of `GOAL`.
- `(fallback GOAL)`: Annotation which transforms a nondeterministic staging-time goal into a deterministic one. It does this by "falling back" to a runtime implementation if the goal is found to be nondeterministic. The erased semantics are identical to that of `GOAL`.
- `(gather GOAL)`: Annotation which transforms a nondeterministic staging-time goal into a deterministic one. It does this by enumerating each potential state, then joining them together as a disjunction. Note: if `GOAL` does not terminate *at staging-time*, then this goal will not either. The erased semantics are identical to that of `GOAL`.
- `(define-partial (NAME REL-NAME [INITIAL-ARGS ...] [FINAL-ARGS ...]) BODY ...)`: Defines a partial relation called `NAME` which is available only at run-time. The `INITIAL-ARGS ...` must be supplied when the goal is partially applied, and `FINAL-ARGS ...` must be supplied when the partially applied goal is applied fully. `REL-NAME` is bound to the partial relation object itself in `BODY ...`. Has no direct analogue in traditional miniKanren.
- `(partial-apply REL-OBJ REL-NAME INITIAL-ARGS ...)`: Partially applies the relation `REL-NAME` to an initial set of arguments `INITIAL-ARGS ...`, unifying `REL-OBJ` with the newly created partially-applied relation. Has no direct analogue in traditional miniKanren.
- `(specialize-partial-apply REL-OBJ REL-NAME INITIAL-ARGS ...)`: Partially applies and specialized the relation `REL-NAME` to an initial set of arguments `INITIAL-ARGS ...`, unifying `REL-OBJ` with the newly created partially-applied relation. The erased semantics are identical to that of `partial-apply`.
- `(finish-apply REL-OBJ REL-NAME FINAL-ARGS ...)`: Finishes the application of a relation object `REL-OBJ`, which must be a partial application of `REL-NAME`, to `FINAL-ARGS ...`. If `REL-OBJ` is fresh, it is unified with a partial application of `REL-NAME` to fresh arguments.

## Correspondences with paper

Here, we map the various claims and examples posited in the paper to corresponding tests/benchmarks in the implementation. 

### Introduction

The introduction briefly discusses the problems the paper attempts to solve, the mechanisms for solving them, as well as the contributions of the paper. It does not make major references to the implementation itself, mainly containing pointers to further into the paper. It does include Figure 1, which presents the `synth/sketch` and `invert-execute` constructs. These are syntactic sugar for various applications of the relational interpreter. The definition of and tests using the `synth/sketch` and `invert-execute` forms are in [`./tests/synth-task-macros-synth-context.rkt`](./tests/synth-task-macros-synth-context.rkt) and [`./tests/synth-task-macros-backwards.rkt`](./tests/synth-task-macros-backwards.rkt), respectively. 

### Background

#### Relational Programming in miniKanren

This section introduces prior work on the relational programming language miniKanren. The short example present in the paper used to introduce the language miniKanren is present as a test in [`./tests/doc.rkt`](./tests/doc.rkt). This section also contains Figure 2b, an implementation of the λ-or language as a relation, demonstrating the use of forms such as quasi-quote and unquote. This implementation is also present in [`./tests/or-interp-unstaged.rkt`](./tests/or-interp-unstaged.rkt), including a short test demonstrating synthesis of terms in the language.

##### Synthesis with Relational Interpreters

This section introduces the use of relational interpreters, both for synthesis and program inversion. It includes Figure 3, which demonstrates how the examples of Figure 1 are implemented in terms of the relational interpreter. These definitions, as well as a number of variations thereof, are present in [`./tests/doc-bench.rkt`](./tests/doc-bench.rkt) and [`tests/doc.rkt`](tests/doc.rkt), respectively. The short example of synthesizing the `lambda` keyword is in [`./tests/or-interp-unstaged.rkt`](./tests/or-interp-unstaged.rkt).

#### Program Staging

This section introduces traditional functional program staging using the MetaOCaml programming language. It includes Figure 4, an implementation of the language from Figure 2 in MetaOCaml, using staging to remove the overhead of interpretation. MetaOCaml can be downloaded using the [`opam` package manager](https://opam.ocaml.org/) for OCaml projects. With `opam` installed, MetaOCaml can be installed using the following commands, a modified version of those from the [MetaOCaml Website](https://okmij.org/ftp/ML/MetaOCaml.html#install):

```
opam update
opam switch add 4.14.1+BER
opam switch set 4.14.1+BER
eval `opam config env`
```

This should provide the `metaocaml` command, which runs MetaOCaml source code. The following commands run the example:

```sh
cd ./tests
metaocaml ./or-interp.ml
```

### Introduction to Multi-stage miniKanren

This section informally introduces the syntax and semantics of multi-stage miniKanren language, mainly through use of prose and examples. The first example, which introduces the `staged` and `later` annotations, is Figure 5. A test for this figure is present in [`./tests/doc.rkt`](./tests/doc.rkt). Tests for the short `pet` relation example are available in the same file. 

Figures 6 and 7 present a number of variations of the `noto` relation, demonstrating the different mechanisms for dealing with non-determinism. The definitions and tests for each variation are in [`./tests/doc-noto.rkt`](./tests/doc-noto.rkt).

#### Staged Interpreters in Multi-stage miniKanren

This section introduces the practice of writing interpreters in multi-stage miniKanren using a staged variant of the λ-or interpreter presented in Figure 2. Figure 8 gives the definition of the staged interpreter. This is equivalent to that of the prior interpreter, but now with added annotations. The definition of this interpreter and tests for it (including the example used in the paragraph before) are in [`./tests/interp-doc.rkt`](./tests/interp-doc.rkt).

##### Handling non-determinism

This section introduces the mechanisms by which nondeterminism can arise in relational interpreters; namely by the presence of non-ground staging-time terms or the use of staging-time nondeterminism to remove staging-time control-flow. The examples in this paper which demonstrate both of these phenomena are also in [`./tests/interp-doc.rkt`](./tests/interp-doc.rkt).

##### Sharing code for abstractions

This section introduces the use of higher-order relations in multi-stage miniKanren through the `partial-apply`, `finish-apply` and `specialize-partial-apply` forms. In staged relational interpreters, higher-order relations are often used to represent first-class function objects, including those in the λ-or interpreter, as shown in Figure 9. Figure 10 demonstrates how function application in the λ-or language corresponds to partially-applied relations in the generated code. Code for both figures is present in [`./tests/interp-doc.rkt`](./tests/interp-doc.rkt).

### Semantics

This section gives a formal syntax and semantics to a subset of multi-stage miniKanren. The syntax is given as a grammar in Figure 11, and the semantics are given in Figure 13 in terms of the stream abstraction described in Figure 12.

The project defines the syntax of multi-stage miniKanren using a `syntax-spec` language in [`main.rkt`](./main.rkt). That file dispatches each of the different syntactic forms present in Figure 13 in the paper to their corresponding internal implementations. The $$g\_r$$ nonterminal corresponds to the `compile-runtime-goal` DSL syntax, the $$g\_s$$ nonterminal corresponds to the `compile-now-goal` DSL syntax, and the $$g\_l$$ nonterminal corresponds to the `compile-later-goal` DSL syntax. Each of these calls different procedures prefixed with `i:`, which refer to different parts of the internal representation.

The actual miniKanren implementation which is run once at staging-time and once at run-time is a [modified version of the existing `faster-minikanren` project](./private/faster-minikanren), which implements both the stream operations presented in the paper and the traditional miniKanren goal-constructors such as `fresh`, `conde`, `==`, as well as others (see [`private/faster-minikanren/mk.scm`](./private/faster-minikanren/mk.scm)). It also includes support for unifying against the new `apply-rep` structure type, which represents partial relation applications, but is not present in the semantics. The [`private/internals.rkt`](./private/internals.rkt) file implements the new goal-constructors added to multi-stage miniKanren, such as `fallback` and `gather` (which are part of the semantics), and partial relation application and specialization (which are not). The `capture-syntax`, `generate-syntax` and `erase` metafunctions do not have direct analogues in the implementation due to complications from other concerns, such as the implicit lifting of unifications as described in Section 3.1. Similarly, there are analogues of the examples presented in Figures 14 and 15.

### Removing Interpretive Overhead from Relational Queries with Staging

#### Interpreting Functions Relationally

This section presents how functions written in languages defined by staged relational interpreters can be used as relations without interpretive overhead. Figure 16 presents a staged adaptation of the second example of Figure 3. A hand-written version of the `appendo` relation, a version written as a function in a unstaged interpreter, as well as a version written in a staged interpreter are available in [`./tests/interpreter/basics.rkt`](./tests/interpreter/basics.rkt). Evidence for the claim that the version written in a staged interpreter is nearly as efficient as the hand-written version is given in section 6.

#### Accelerating Program Synthesis by Sketch

This section presents how program synthesis using relational interpreters can be accelerated by the use of staging. It includes Figure 17, a staged adaptation of the first example of Figure 3. Several versions of the `fib` function, including the one in Figure 17, are present in [`./tests/applications/peano-fib.rkt`](./tests/applications/peano-fib.rkt). 

#### Other Staged Interpreters

This section includes two further applications of staging to relational interpretation: a relational interpreter for miniKanren itself, as well as a relational parsing mechanism. The source of both applications, including the listed examples, are present in [`./tests/applications/metamk.rkt`](./tests/applications/metamk.rkt) and [`./tests/applications/grammars.rkt`](./tests/applications/grammars.rkt), respectively.

### Evaluation

This section presents Figure 19, a table listing a number of benchmarks, showing the time taken by staged and unstaged versions of various multi-stage miniKanren queries. All of these benchmarks can be run in sequence by running [`./tests/bench-paper.rkt`](./tests/bench-paper.rkt). Each of the benchmarks prints debug information and results to `STDOUT`. To generate the exact figure used in the paper, the `benchrun.sh` bash file runs the tests, saves the results to a file, then runs the python script `benchread.py`. This script reads that file, parses the results and outputs the tex used in the figure.

The benchmarks should run in less than an hour on common commercial hardware, but can be shortened if need be. In order to decrease the timeout limit on the larger tests, modify the `timeout-limit-seconds` variable of [`./test-check.rkt`](./test-check.rkt). 

#### Comparison to a Hand-Tuned Relational Interpreter

This section presents Figure 20, a table comparing the synthesis speeds of the staged relational interpreter and of the hand-tuned relational interpreter presented in [Byrd et. al's 2017 paper](https://doi.org/10.1145/3110252), called Barliman. The code which runs and times the Barliman interpreter is in the [`tests/barliman-comparison`](tests/barliman-comparison) sub-directory. The figure itself is written by hand; see [`tests/barliman-comparison/README.md`](tests/barliman-comparison/README.md) for more information. 
