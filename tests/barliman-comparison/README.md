# Barliman Comparison

This directory contains code and queries used for comparing Barliman against the optimized `evalo` interpreter (`evalo-optimized.scm`) from the artifact accompanying the paper "A Unified Approach to Solving Seven Programming Problems" (Oxford, ICFP 2017).

Many files included here are directly taken from that artifact.

## Quick Start

To load and run the comparison script:

```scheme
$ chez
Chez Scheme Version 10.1.0
Copyright 1984-2024 Cisco Systems, Inc.

> (load "barliman-comparison.scm")
```

## Directory Contents

### Unmodified Files from Oxford '17 Artifact

The following files are unchanged and maintain the same relative paths as in the original Oxford artifact. Their descriptions can be found in the artifact itself:

- `challenge-7.scm`
- `evalo-optimized.scm`
- `evalo-standard.scm`
- `mk/mk.scm`
- `mk/test-check.scm`

### Files from Barliman Repository

The following files were sourced directly from Barliman pull request [#32](https://github.com/webyrd/Barliman/pull/32):

- `test-fib-aps-synth.scm`
- `test-proofo.scm`

### Lightly Modified Source Files

The following files were adapted slightly to integrate the Oxford artifact environment:

- **`chez-load-interp.scm`**: Based on [Barliman's `chez-load-interp.scm`](https://github.com/webyrd/Barliman/blob/master/cocoa/Barliman/mk-and-rel-interp/chez-load-interp.scm), but modified to use the Oxford ICFP17 versions of `test-check`, `arithmetic.scm`, and the optimized interpreter (`evalo-optimized.scm`).

- **`mk/arithmetic.scm`**: Originally from the Oxford artifact, modified to adjust the load path of MiniKanren to reference `mk/mk.scm`.

### New Files

- **`barliman-comparison.scm`**: Main script for running all comparison queries.

