# Multi-stage miniKanren

## Abstract

We introduce multi-stage miniKanren, which augments miniKanren with staging constructs that allow us to have precise control over the staging process. We have novel constructs to account for non-determinism. We use multi-stage miniKanren to stage interpreters written as relations, in which the programs under interpretation can contain holes representing unknown values. We apply this staging framework to a relational interpreter for a subset of Racket, and demonstrate significant performance gains across multiple synthesis problems.

## Getting started with Docker (optional)

The docker image can be pulled with `git pull namin/staged-minikanren` or build here with `docker build -t namin/staged-minikanren .`.

Note: on Mac, _disable_ Rosetta in the Docker Desktop settings. [See this thread.](https://racket.discourse.group/t/racket-docker-m1-rosetta/2947/6)

An interactive Racket session can be ran with `docker run -it namin/staged-minikanren` and the benchmarks can be ran with the additional argument `/app/staged-miniKanren/benchrun.sh`.

For the interactive session, try a sanity check with the following lines:
```
> (require "all.rkt")
> (run 1 (q) (staged (== q 'fun)))
'(fun)
```

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

## Contents

### Frontend

The project defines the syntax of multi-stage miniKanren using a `syntax-spec` language in [`main.rkt`](./main.rkt). That file dispatches each of the different syntactic forms present in Figure 13 in the paper to their corresponding internal implementations. The $$g\_r$$ nonterminal corresponds to the `compile-runtime-goal` DSL syntax, the $$g\_s$$ nonterminal corresponds to the `compile-now-goal` DSL syntax, and the $$g\_l$$ nonterminal corresponds to the `compile-later-goal` DSL syntax. Each of these calls different procedures prefixed with `i:`, which refer to different parts of the internal representation.

### Backend

The actual miniKanren implementation which is run once at staging-time and once at run-time is a [modified version of the existing `faster-minikanren` project](./private/faster-minikanren), which implements the traditional miniKanren goal-constructors such as `fresh`, `conde`, `==`, and others (see [`private/faster-minikanren/mk.scm`](./private/faster-minikanren/mk.scm)), but also includes support for unifying against the new `apply-rep` structure type, which represents partial relation applications. The [`private/internals.rkt`](./private/internals.rkt) file implements the new goal-constructors added to multi-stage miniKanren, such as `fallback`, `gather`, and partial relation application and specialization.

### Relational Interpreters

There are a number of relational interpreters for different languages in this project. The largest interpreter is for the subset of racket used in Byrd et. al; it is used in Figures 19, 20 and 21 to demonstrate relational synthesis. The version of this interpreter without annotations is in [`unstaged-interp.scm`](./unstaged-interp.scm), and the version with staging annotations is in [`staged-interp.scm`](./staged-interp.scm). Interpreters for a smaller dialect of racket are present in the [`small-interp`](./small-interp.scm) directory. The relational interpreter for the λ-or language from Figures 10 and 11 of the paper is defined in [`tests/or-interp.rkt`](./tests/or-interp.rkt).

### Case-Studies

The case-studies presented in section 6.1 of the paper are present in the [`tests/applications/`](./tests/applications) sub-directory. These include:
- [parsing with derivatives](./tests/applications/parsing-with-derivatives.rkt)
- [Theorem checker turned prover](./tests/applications/proof.rkt)
- [Negation Normal Form (NNF)](./tests/applications/dl.rkt)
- [Peano](./tests/applications/peano-fib.rkt)
- [Synthesis](./tests/doc-bench.rkt)
- [Double Evaluators](./tests/applications/double-eval.rkt)
- [miniKanren-in-miniKanren](./tests/applications/metamk.rkt)
- [Grammar Parsers](./tests/applications/grammars.rkt)

### Tests and Plots

Tests for the project are available under the [`tests/`](./tests/) directory. Running the file [`tests/all.rkt`](./tests/all.rkt) will run the tests. To replicate the benchmarks as they appear in the paper, run the shell script [`benchrun.sh`](./benchrun.sh), which will run the tests, printing raw results to `STDOUT` and to log files of the form `bench-log-*.txt` and printing formatted results to `bench-results/*`. 

The plots are generated by running [`tests/graphs.rkt`](./tests/graphs.rkt). 
