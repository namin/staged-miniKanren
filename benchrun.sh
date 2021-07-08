#!/bin/bash

set -e

chez --script tests-bench-rel.scm | tee bench-log-ex.txt
python benchread.py >../staged-mk-paper/bench_rel.tex
cp bench-log-ex.txt bench-log-ex-rel.txt

chez --script tests-doc-bench.scm | tee bench-log-ex.txt
python benchread.py >../staged-mk-paper/bench_doc.tex
cp bench-log-ex.txt bench-log-ex-doc.txt

chez --script tests-double-eval.scm | tee bench-log-ex.txt
python benchread.py >../staged-mk-paper/bench_double_eval.tex
cp bench-log-ex.txt bench-log-ex-double-eval.txt
