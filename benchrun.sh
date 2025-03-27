#!/bin/bash

set -e

if [ -d "../staged-mk-paper" ]; then
    BENCH_DIR="../staged-mk-paper"
else
    mkdir -p bench-results
    BENCH_DIR="./bench-results"
fi

xvfb-run racket -y tests/bench-paper.rkt | tee bench-log-ex.txt
python3 benchread.py >$BENCH_DIR/bench_paper.tex
cp bench-log-ex.txt bench-log-ex-paper.txt

# Charts show up in:
# grammars.rkt
# proofs.rkt
# basics.rkt
