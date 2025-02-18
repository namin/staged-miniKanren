#!/bin/bash

set -e

if [ -d "../staged-mk-paper" ]; then
    BENCH_DIR="../staged-mk-paper"
else
    mkdir -p bench-results
    BENCH_DIR="./jbh_bench-results"
fi

racket tests/bench-paper.rkt | tee bench-log-ex.txt
python3 jbh_benchread.py >$BENCH_DIR/bench_paper.tex
cp bench-log-ex.txt bench-log-ex-paper.txt

# Charts show up in:
# jbh_grammars.rkt
# jbh_proofs.rkt
# basics.rkt
