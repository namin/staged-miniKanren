# staged-miniKanren

A novel framework for staging interpreters written as relations, in which the programs under interpretation are allowed to contain holes representing unknown values. We apply this staging framework to a relational interpreter for a subset of Racket, and demonstrate significant performance gains across multiple synthesis problems.

## Installation

- `git submodule init`
- `git submodule update`
- Install as Racket package, including dependencies, with `raco pkg install` in this directory.

## Running
- `racket tests/all.rkt`
- `racket tests/all-bench.rkt`
