#!/bin/bash

set -e

rm -rf release
mkdir release
mkdir release/staged-miniKanren
mkdir release/faster-miniKanren
cd release

cp ../../faster-miniKanren/mk-vicare.scm faster-miniKanren/
cp ../../faster-miniKanren/mk.scm faster-miniKanren/

cp ../RUN.md README.md
cp ../benchread.py staged-miniKanren/

cp ../staged-load.scm staged-miniKanren/
cp ../staged-run.scm staged-miniKanren/
cp ../staged-utils.scm staged-miniKanren/
cp ../staged-interp.scm staged-miniKanren/
cp ../unstaged-interp.scm staged-miniKanren/
cp ../staged-shaker.scm staged-miniKanren/

cp ../tests-append.scm staged-miniKanren/

cp ../test-check.scm staged-miniKanren/
cp ../tests-all.scm staged-miniKanren/
cp ../tests-all-bench.scm staged-miniKanren/
cp ../tests-bench-rel.scm staged-miniKanren/
cp ../tests-doc-bench.scm staged-miniKanren/
cp ../tests-doc.scm staged-miniKanren/
cp ../tests-dl.scm staged-miniKanren/
cp ../tests-double-eval.scm staged-miniKanren/
cp ../tests-peano-fib.scm staged-miniKanren/
cp ../tests-parsing-with-derivatives.scm staged-miniKanren/
cp ../tests-proof.scm staged-miniKanren/
cp ../tests-run.scm staged-miniKanren/
cp ../tests.scm staged-miniKanren/

cd staged-miniKanren
chez --script tests-all.scm

cd ..

egrep "namin|WEB" */*
