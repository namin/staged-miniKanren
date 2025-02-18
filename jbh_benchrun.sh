#!/bin/bash

set -e

if [ -f "../staged-mk-paper" ]; then
    BENCH_DIR="../staged-mk-paper"
else
    mkdir -p bench-results
    BENCH_DIR="./jbh_bench-results"
fi

racket tests/bench-paper.rkt | tee bench-log-ex.txt
python3 benchread.py >$BENCH_DIR/bench_paper.tex
cp bench-log-ex.txt bench-log-ex-paper.txt


# These must be the ones from the document.
# racket tests/doc-bench.rkt | tee bench-log-ex.txt
# python3 benchread.py >$BENCH_DIR/bench_doc.tex
# cp bench-log-ex.txt bench-log-ex-doc.txt


# These belong, among others, in the benchmark table for the paper.
racket tests/applications/double-eval.rkt | tee bench-log-ex.txt
python3 benchread.py >$BENCH_DIR/bench_double_eval.tex
cp bench-log-ex.txt bench-log-ex-double-eval.txt

# Too much awesome for this paper.
# racket tests/applications/metaKanren.rkt | tee bench-log-ex.txt
# python3 benchread.py >$BENCH_DIR/bench_metaKanren.tex
# cp bench-log-ex.txt bench-log-ex-metaKanren.txt
