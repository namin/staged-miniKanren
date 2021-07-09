# staged-miniKanren

## Run

We use Chez Scheme for `scheme`. It could be called `chez` depending on the system.

All commands are executed from the `staged-miniKanren` directory.

Run the small tests with `scheme tests-all.scm`.

Run the benchmarks with `scheme tests-all-bench.scm | tee bench-log-ex.txt`.
Then run `python benchread.py` to generate the table entries.

The small examples from the draft are in `tests-doc.scm`.
