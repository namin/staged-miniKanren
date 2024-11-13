# Multi-stage miniKanren Code Artifact

## Abstract

We introduce multi-stage miniKanren, which augments miniKanren with staging constructs that allow us to have precise control over the staging process. We have novel constructs to account for non-determinism. We use multi-stage miniKanren to stage interpreters written as relations, in which the programs under interpretation can contain holes representing unknown values. We apply this staging framework to a relational interpreter for a subset of Racket, and demonstrate significant performance gains across multiple synthesis problems.

## Dependencies

Multi-Stage miniKanren is a [Racket](https://racket-lang.org/) library which relies on two existing pieces of software: [syntax-spec-v2](https://pkgs.racket-lang.org/package/syntax-spec-v2), a metalanguage for implementing DSLs in Racket; and a modified version of [faster-minikanren](https://github.com/michaelballantyne/faster-minikanren), an efficient implementation of miniKanren with constraint solving. The former dependency can be installed using Racket's builtin package manager, [raco](https://docs.racket-lang.org/raco/index.html), by running the shell command:

```shell
raco pkg install syntax-spec-v2
```

The latter dependency is bundled with this artifact, under `private/faster-minikanren`. No work should be necessary to build it separately.

## Contents

The artifact includes an implementation of multi-stage miniKanren as a layer atop the existing `faster-minikanren` implementation. Staging-time evaluation and run-time evaluation are two distinct evaluations of `faster-minikanren` queries. The front-end syntax is implemented as a `syntax-spec` language in [`main.rkt`](./main.rkt).

Tests for the artifact are available under the [`tests/`](./tests/) directory. Running the file [`tests/all.rkt`](./tests/all.rkt) will run the tests. To replicate the benchmarks as they appear in the paper, run the shell script [`benchrun.sh`](./benchrun.sh), which will run the tests, printing raw results to `STDOUT` and to log files of the form `bench-log-*.txt` and printing formatted results to `bench-results/*`.
