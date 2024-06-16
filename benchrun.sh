#!/bin/bash

set -e

racket tests/bench-rel.rkt | tee bench-log-ex.txt
python3 benchread.py >../staged-mk-paper/bench_rel.tex
cp bench-log-ex.txt bench-log-ex-rel.txt

racket tests/doc-bench.rkt | tee bench-log-ex.txt
python3 benchread.py >../staged-mk-paper/bench_doc.tex
cp bench-log-ex.txt bench-log-ex-doc.txt

racket tests/applications/double-eval.rkt | tee bench-log-ex.txt
python3 benchread.py >../staged-mk-paper/bench_double_eval.tex
cp bench-log-ex.txt bench-log-ex-double-eval.txt

racket tests/applications/metaKanren.rkt | tee bench-log-ex.txt
python3 benchread.py >../staged-mk-paper/bench_metaKanren.tex
cp bench-log-ex.txt bench-log-ex-metaKanren.txt
