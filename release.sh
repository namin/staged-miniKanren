#!/bin/bash

set -e

rm -rf release
mkdir release
mkdir release/staged-miniKanren
cd release
git clone https://github.com/namin/faster-miniKanren.git
cd faster-miniKanren
git checkout staged
rm -rf .git
cd ..

cp ../RUN.md README.md
cp ../benchread.py staged-miniKanren/

cp ../staged-load0.scm staged-miniKanren/
cp ../staged-load.scm staged-miniKanren/

cp ../staged-run.scm staged-miniKanren/
cp ../staged-utils.scm staged-miniKanren/
cp ../staged-interp.scm staged-miniKanren/
cp ../unstaged-interp.scm staged-miniKanren/
cp ../staged-shaker.scm staged-miniKanren/

cp ../test-check.scm staged-miniKanren/
cp ../tests-all.scm staged-miniKanren/
cp ../tests-all-bench.scm staged-miniKanren/
cp ../tests-bench.scm staged-miniKanren/
cp ../tests-doc-bench.scm staged-miniKanren/
cp ../tests-doc.scm staged-miniKanren/
cp ../tests-dl.scm staged-miniKanren/
cp ../tests-micro.scm staged-miniKanren/
cp ../tests-peano-fib.scm staged-miniKanren/
cp ../tests-parsing-with-derivatives.scm staged-miniKanren/
cp ../tests-run.scm staged-miniKanren/
cp ../tests.scm staged-miniKanren/

cd staged-miniKanren
chez --script tests-all.scm

cd ..

egrep "namin|WEB" */*
